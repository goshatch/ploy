{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Error handling
module SchemeError
  ( SchemeError (..),
    ThrowsError,
    IOThrowsError,
    throwError,
    catchError,
    liftThrows,
    runIOThrows,
    extractValue,
  )
where

import Control.Monad.Except
import Data.Text (Text)
import Data.Text qualified as T
import SchemeTypes

data SchemeError
  = -- | Wrong number of args
    NumArgs !Integer ![LispVal]
  | -- | Expected one type but got another
    TypeMismatch !Text !LispVal
  | -- | Parse error (w/ error message)
    Parser !Text
  | -- | Malformed special form syntax
    BadSpecialForm !Text !LispVal
  | -- | Tried to call something that's not a function
    NotFunction !Text !Text
  | -- | Referenced an undefined var
    UnboundVar !Text !Text
  | -- | Tried to divide by zero
    DivisionByZero
  | -- | Generic error w/ message
    Default !Text
  deriving stock (Show, Eq)

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

-- | This is a type alias for operations that might fail
-- `Either SchemeError a` means the operation will either produce an error or a
-- value of type 'a'
type ThrowsError = Either SchemeError

-- | Type for IO operations that might fail
-- `ExceptT` is a monad transformer that adds error handling to IO
type IOThrowsError = ExceptT SchemeError IO

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
