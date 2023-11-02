module Main where

import Actions (loadTasks, startMonitor)
import System.Directory (getCurrentDirectory)

main :: IO ()
main = do
  cwd <- getCurrentDirectory
  loadTasks (cwd <> "/examples/multiseq.timo") >>= startMonitor
