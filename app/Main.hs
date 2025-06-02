module Main (main) where

import System.Environment (getArgs)
import Repl

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> runRepl
    [filename] -> runFile filename
    _ -> putStrLn "Usage: ploy [filename]"
