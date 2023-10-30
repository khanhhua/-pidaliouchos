module Data.Timonieris where

import GHC.Conc ( ThreadId )
import Control.Concurrent.MVar ( MVar, newMVar )

type TaskId = Integer

data Monitor = Monitor
  { tasks :: MVar [(Integer, ThreadId, MVar ())] 
  , nextTaskId :: MVar TaskId
  }
data Task = SimpleTask String

defaultMonitor :: IO Monitor
defaultMonitor = do
  empty <- newMVar []
  nextTaskId <- newMVar 1
  pure $ Monitor empty nextTaskId

simple :: String -> Task
simple =
  SimpleTask
