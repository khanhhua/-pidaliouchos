{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use list literal pattern" #-}
module Actions ( loadTasks, startMonitor ) where

import Control.Concurrent
    ( ThreadId
    , MVar
    , forkIO
    , newEmptyMVar
    , putMVar
    , takeMVar
    , modifyMVar_
    , readMVar
    )
import Control.Monad ( mapM_ )
import Control.Monad.Trans.Reader ( ReaderT, ask, runReaderT )
import Control.Monad.IO.Class ( liftIO );

import System.Directory ( getCurrentDirectory )
import System.Exit ( ExitCode(..) )
import System.Process ( spawnProcess, waitForProcess )

import Data.List ( singleton )

import Data.Timonieris
    ( TaskId
    , Task(..)
    , Monitor(..)
    , defaultMonitor
    , simple
    , parameterized
    , block
    )

type TaskT = ReaderT Monitor IO Task


loadTasks :: IO [Task]
loadTasks = do
  cwd <- getCurrentDirectory
  let timofile = cwd <> "/examples/multiseq.timo"
  content <- lines <$> readFile timofile
  pure $ parseTimo cwd content      

startMonitor :: [Task] -> IO ()
startMonitor tasks = do
  monitor <- defaultMonitor
  let
    taskListT = map fromTask tasks
    runWithMonitor = flip runReaderT monitor
  mapM_ runWithMonitor taskListT
  wait monitor
  putStrLn "Done."


-- Private functions

fromTask :: Task -> TaskT
fromTask task = do
  monitor <- ask
  liftIO $ do
    runTask monitor task
    pure task


fromCwdPath :: String -> TaskT
fromCwdPath executablePath = do
  monitor <- ask
  liftIO $ do
    cwd <- getCurrentDirectory
    let task = simple $ cwd <> executablePath

    runTask monitor task
    pure task


runTask :: Monitor -> Task -> IO ()
runTask monitor task0 = do
  (taskId, handle) <- createTaskId monitor
  threadId <- forkIO $ worker taskId task0
  
  watch (taskId, threadId, handle) monitor

  where
    spawnTask task = do
      case task of
        SimpleTask executable -> spawnProcess executable [] >>= waitForProcess
        ParameterizedTask executable params -> spawnProcess executable params >>= waitForProcess
        _ -> error "Bad task"

    worker taskId task0 =
      case task0 of
        BlockTask task next -> do
          subtaskExitCode <- spawnTask task
          case subtaskExitCode of
            ExitSuccess -> worker taskId next
            ExitFailure code -> do
              putStrLn "Subtask crashed. Respawning"
              worker taskId task0
          -- 
        _ -> do
          exitCode <- spawnTask task0
          case exitCode of
            ExitSuccess -> success taskId monitor
            ExitFailure code -> do
              putStrLn "Task crashed. Respawning"
              worker taskId task0


watch :: (Integer, ThreadId, MVar ()) -> Monitor -> IO ()
watch (taskId, threadId, handle) (Monitor{..}) = do
  modifyMVar_ tasks (\xs -> pure $ (taskId, threadId, handle) : xs)


wait :: Monitor -> IO ()
wait (Monitor{..}) = do
  vTasks <- readMVar tasks
  putStrLn $ "Tasks pending " ++ show (length vTasks)
  mapM_ (\(_,_, handle) -> takeMVar handle) vTasks


createTaskId :: Monitor -> IO (Integer, MVar ())
createTaskId (Monitor {..}) = do
  taskId <- readMVar nextTaskId
  handle <- newEmptyMVar
  modifyMVar_ nextTaskId (pure . (+) 1)

  pure (taskId, handle)


success :: TaskId -> Monitor -> IO ()
success taskId (Monitor{..}) = do
  vTasks <- readMVar tasks
  let (_, _, handle) = head $ filter (\(t, _, _) -> t == taskId) vTasks
  putMVar handle ()


parseTimo cwd content =
  foldr (\tasks acc ->
    case tasks of
      [task] -> task : acc
      _ -> block tasks : acc
    ) [] singleTasks
  where
    singleTasks = foldr (\line acc ->
      if line == "" then [] : acc
      else
        let task = parseLine (cwd <> "/" <> line) 
            xs = if null acc
                  then []
                  else head acc
        in case acc of
          [] -> [ task : xs ]
          _  -> ( task : xs ) : tail acc
      ) [] content


parseLine line =
  case words line of
    executable : [] -> simple executable
    executable : params -> parameterized executable params
    _ -> error "Nah you are on crack"
