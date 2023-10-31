module Main where

import Actions ( loadTasks, startMonitor )

main :: IO ()
main =
  loadTasks >>= startMonitor



