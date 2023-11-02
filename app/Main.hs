module Main where

import Actions (loadTasks, startMonitor)
import System.Directory (getCurrentDirectory, getHomeDirectory)
import System.Environment (getArgs)

main :: IO ()
main =
  resolveTimofile >>= loadTasks >>= startMonitor

resolveTimofile :: IO FilePath
resolveTimofile = do
  args <- getArgs
  case args of
    [timofile] -> (\cwd -> cwd <> "/" <> timofile) <$> getCurrentDirectory
    [] -> do
      home <- getHomeDirectory
      pure $ home <> "/.timonieris"
