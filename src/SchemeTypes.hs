{-# LANGUAGE DerivingStrategies #-}

module SchemeTypes where

import Data.Text (Text)
import qualified Data.Text as T

-- | Core data type representing any Scheme value
-- Each constructor represents a different type of value in Ploy
data LispVal
  = Atom Text                    -- ^ A symbol, like 'foo or 'bar
  | List [LispVal]               -- ^ A proper list: (1 2 3)
  | DottedList [LispVal] LispVal -- ^ An improper list: (1 2 . 3)
  | Number Integer               -- ^ A whole number
  -- TODO: Float support
  | String Text                  -- ^ A string literal
  | Bool Bool                    -- ^ #t or #f
  | Nil                          -- ^ The empty list '()
  deriving stock (Eq)            -- We can compare LispVals for equality

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
showVal (DottedList heads tail) =
  "(" <> T.unwords (map showVal heads) <> " . " <> showVal tail <> ")"

-- Make LispVal an instance of Show for debugging
instance Show LispVal where
  show = T.unpack . showVal
