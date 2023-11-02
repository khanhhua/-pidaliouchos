{-# LANGUAGE InstanceSigs #-}

module Data.Timonieris where

import Data.Semigroup

import Control.Concurrent.MVar (MVar, newMVar)
import GHC.Conc (ThreadId)
import System.Directory (Permissions (executable))

type TaskId = Integer
type ExecutablePath = String

data Monitor = Monitor
  { tasks :: MVar [(Integer, ThreadId, MVar ())]
  , nextTaskId :: MVar TaskId
  }
data Task
  = SimpleTask ExecutablePath
  | ParameterizedTask ExecutablePath [String]
  | BlockTask (ExecutablePath, [String]) Task

instance Semigroup Task where
  (<>) :: Task -> Task -> Task
  (SimpleTask executable) <> task = BlockTask (executable, []) task
  (ParameterizedTask executable params) <> task = BlockTask (executable, params) task
  (BlockTask (executable, params) subtask) <> task = BlockTask (executable, params) $ subtask <> task

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
block [x] = x
block (x : xs) = x <> block xs
