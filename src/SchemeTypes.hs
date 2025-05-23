{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module SchemeTypes (LispVal(..), showVal) where

import Data.Text (Text)
import Data.Text qualified as T

-- | Core data type representing any Scheme value
-- Each constructor represents a different type of value in Ploy
data LispVal
  = -- | A symbol, like 'foo or 'bar
    Atom Text
  | -- | A proper list: (1 2 3)
    List [LispVal]
  | -- | An improper list: (1 2 . 3)
    DottedList [LispVal] LispVal
  | -- | A whole number
    -- TODO: Float support
    Number Integer
  | -- | A string literal
    String Text
  | -- | #t or #f
    Bool Bool
  | -- | The empty list '()
    Nil
  deriving stock (Eq) -- We can compare LispVals for equality?

-- | Convert a LispVal to its textual representation
-- This is how values are displayed in the REPL
showVal :: LispVal -> Text
showVal (Atom name) = name
showVal (String txt) = "\"" <> txt <> "\""
showVal (Number n) = T.pack (show n)
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal Nil = "()"
showVal (List vals) = "(" <> T.unwords (map showVal vals) <> ")"
showVal (DottedList heads tails) =
  "(" <> T.unwords (map showVal heads) <> " . " <> showVal tails <> ")"

-- Make LispVal an instance of Show for debugging
instance Show LispVal where
  show = T.unpack . showVal
