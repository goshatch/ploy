{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module SchemeTypes (LispVal (..), showVal) where

import Data.Text (Text)
import Data.Text qualified as T
import Environment (EnvRef)
import SchemeError (IOThrowsError)

-- | Core data type representing any Scheme value
-- Each constructor represents a different type of value in Ploy
data LispVal
  = -- | A symbol, like 'foo or 'bar
    Atom !Text
  | -- | A proper list: (1 2 3)
    List [LispVal]
  | -- | An improper list: (1 2 . 3)
    DottedList [LispVal] !LispVal
  | -- | A whole number
    -- TODO: Float support
    Number !Integer
  | -- | A string literal
    String !Text
  | -- | #t or #f
    Bool !Bool
  | -- | The empty list '()
    Nil
  | PrimitiveFunc ([LispVal] -> IOThrowsError Lispval)
  | -- | Params, body, closure
    Function [Text] LispVal EnvRef
  deriving stock (Eq, Show, Ord) -- We can compare LispVals for =/>/< etc?

-- | Convert a LispVal to its textual representation
-- This is how values are displayed in the REPL
showVal :: LispVal -> Text
showVal = \case
  Atom name -> name
  String txt -> "\"" <> txt <> "\""
  Number n -> T.pack (show n)
  Bool True -> "#t"
  Bool False -> "#f"
  Nil -> "()"
  List vals -> "(" <> T.unwords (map showVal vals) <> ")"
  DottedList heads tails ->
    "(" <> T.unwords (map showVal heads) <> " . " <> showVal tails <> ")"

-- NOTE: Instead of deriving stock Show, we can implement our own. This will
-- print using showVal, so output will look like Scheme code.

-- Make LispVal an instance of Show for debugging
-- instance Show LispVal where
--   show = T.unpack . showVal
