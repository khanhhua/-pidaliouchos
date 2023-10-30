module Main where

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

main :: IO ()
main = do
  tasks <- loadTasks
  if length tasks == 0
    then putStrLn "Now tasks available. Now exiting."
    else startMonitor tasks


startMonitor :: [Task] -> IO ()
startMonitor tasks = do
  monitor <- defaultMonitor
  mapM_ (runTask monitor) tasks
  wait monitor
  putStrLn "Done."


watch :: (Integer, ThreadId, MVar ()) -> Monitor -> IO ()
watch (taskId, threadId, handle) (Monitor{..}) = do
  modifyMVar_ tasks (\xs -> pure $ (taskId, threadId, handle) : xs)


wait :: Monitor -> IO ()
wait (Monitor{..}) = do
  vTasks <- readMVar tasks
  putStrLn $ "Tasks pending " ++ (show $ length vTasks)
  mapM_ (\(_,_, handle) -> takeMVar handle) vTasks


loadTasks :: IO [Task]
loadTasks = pure [ simple "/Users/khanhhua/dev/timonieris/tasks/dummy.sh"
                 , simple "/Users/khanhhua/dev/timonieris/tasks/dummy-err.sh"
                 ]


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

