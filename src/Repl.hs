{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Read, Eval, Print, Repeat^DLoop
module Repl (runRepl, runFile) where

import Control.Monad
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Environment
import ErrorHandling
import Eval
import Parser
import System.IO (NewlineMode (inputNL), hFlush, stdout)
import Text.Megaparsec (errorBundlePretty)
import Types

readPrompt :: Text -> IO Text
readPrompt prompt = do
  TIO.putStr prompt
  hFlush stdout
  TIO.getLine

evalString :: EnvRef -> Text -> IO ()
evalString env expr = do
  let parsed = runParser parseExpr expr
  case parsed of
    Left err -> TIO.putStrLn $ "Parse error: " <> err
    Right val -> do
      result <- runIOThrows $ eval env val
      case result of
        Left err -> TIO.putStrLn $ showError err
        Right val -> TIO.putStrLn $ showVal val

runRepl :: IO ()
runRepl = do
  TIO.putStrLn "Welcome to Ploy, your friendly neighbourhood Scheme."
  TIO.putStrLn "Type :quit to exit"
  env <- primitiveEnv
  replLoop env
  where
    replLoop env = do
      input <- readPrompt "ploy> "
      unless (input == ":quit") $ do
        evalString env input
        replLoop env

runFile :: FilePath -> IO ()
runFile filename = do
  contents <- TIO.readFile filename
  env <- primitiveEnv
  let parsed = runParser parseProgram contents
  case parsed of
    Left err -> TIO.putStrLn $ "Parse error: " <> err
    Right exprs -> evalExprs env exprs
  where
    evalExprs _ [] = return ()
    evalExprs env (expr : rest) = do
      result <- runIOThrows $ eval env expr
      case result of
        Left err -> TIO.putStrLn $ showError err
        Right val -> do
          when (val /= Nil) $ TIO.putStrLn $ showVal val
          evalExprs env rest

runParser :: Parser a -> Text -> Either Text a
runParser p input =
  case parse p "<input>" input of
    Left err -> Left $ T.pack $ errorBundlePretty err
    Right val -> Right val
