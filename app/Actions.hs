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
    )

type TaskT = ReaderT Monitor IO Task


loadTasks :: IO [Task]
loadTasks =
  (\cwd -> simple . ((cwd <> "/") <>) <$> executables) <$> getCurrentDirectory
  where
    executables =
      [ "tasks/dummy.sh"
      , "tasks/dummy.sh"
      , "tasks/dummy-err.sh"
      , "tasks/dummy-err.sh"
      , "tasks/dummy-err.sh"
      , "tasks/dummy-err.sh"
      , "tasks/dummy-err.sh"
      ]


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
runTask monitor (SimpleTask executable) = do
  (taskId, handle) <- createTaskId monitor
  threadId <- forkIO $ worker (taskId, handle)
  watch (taskId, threadId, handle) monitor
  where
    worker (taskId, handle) = do
      processHandle <- spawnProcess executable []
      exitCode <- waitForProcess processHandle
      case exitCode of
        ExitSuccess -> success taskId monitor
        ExitFailure code -> do
          putStrLn "Task crashed. Respawning..."
          worker (taskId, handle)

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
