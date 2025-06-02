{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Error handling
module ErrorHandling
  ( ThrowsError,
    IOThrowsError,
    throwError,
    catchError,
    showError,
    liftThrows,
    runIOThrows,
    extractValue,
  )
where

import Control.Monad.Except
import Data.Text (Text)
import Data.Text qualified as T
import Types (IOThrowsError, SchemeError (..), ThrowsError, showVal)

-- | Print errors in a user-friendly way
showError :: SchemeError -> Text
showError = \case
  NumArgs expected found ->
    "Expected "
      <> T.pack (show expected)
      <> " args, but found "
      <> T.pack (show $ length found)
      <> ": "
      <> T.unwords (map showVal found)
  TypeMismatch expected found ->
    "Invalid type: expected "
      <> expected
      <> ", found "
      <> showVal found
  Parser msg -> "Parse error: " <> msg
  BadSpecialForm msg form ->
    msg <> ": " <> showVal form
  NotFunction msg func ->
    msg <> ": " <> func
  UnboundVar msg varname ->
    msg <> ": " <> varname
  DivisionByZero -> "Division by zero!"
  Default msg -> msg

-- | Lift a `ThrowsError` into `IOThrowsError`
-- This converts pure errors into IO errors.
-- TODO: Not super sure how exactly this works yet
liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows = \case
  Left err -> throwError err -- re-throw in new monad
  Right val -> return val -- wrap success value

-- | Run an IOThrowsError operation
runIOThrows :: IOThrowsError a -> IO (Either SchemeError a)
runIOThrows = runExceptT

-- | Extract value or crash (for REPL)
extractValue :: ThrowsError a -> a
extractValue (Right val) = val
extractValue (Left err) = error $ "Extracting error: " ++ T.unpack (showError err)
