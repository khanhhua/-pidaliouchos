module Data.Timonieris where

import GHC.Conc ( ThreadId )
import Control.Concurrent.MVar ( MVar, newMVar )

type TaskId = Integer
type ExecutablePath = String

data Monitor = Monitor
  { tasks :: MVar [(Integer, ThreadId, MVar ())] 
  , nextTaskId :: MVar TaskId
  }
data Task
  = SimpleTask ExecutablePath
  | ParameterizedTask ExecutablePath [String]
  | BlockTask Task Task

defaultMonitor :: IO Monitor
defaultMonitor = do
  empty <- newMVar []
  nextTaskId <- newMVar 1
  pure $ Monitor empty nextTaskId

simple :: ExecutablePath -> Task
simple =
  SimpleTask

parameterized :: ExecutablePath -> [String] -> Task
parameterized =
  ParameterizedTask

block :: [Task] -> Task
block [] = error "You have reached the identity of the Task Monoid :D"
block (x : []) = x
block (x : xs) = BlockTask x $ block xs
