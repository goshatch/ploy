-- | Base types!
module Types
  ( Parser,
    LispVal (..),
    SchemeError (..),
    ThrowsError,
    IOThrowsError,
    EnvRef (..),
    Env,
    showVal,
  )
where

import Control.Monad.Except
import Data.IORef
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec

-- | The type of Ploy's parser
-- Void means we don't have custom error components
-- Text means we're parsing Text input
type Parser = Parsec Void Text

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
  | PrimitiveFunc ([LispVal] -> IOThrowsError LispVal)
  | -- | Params, body, closure
    Function [Text] LispVal EnvRef
  | -- | The empty list '()
    Nil

instance Eq LispVal where
  (Atom a) == (Atom b) = a == b
  (List a) == (List b) = a == b
  (DottedList a1 b1) == (DottedList a2 b2) = a1 == a2 && b1 == b2
  (Number a) == (Number b) = a == b
  (String a) == (String b) = a == b
  (Bool a) == (Bool b) = a == b
  Nil == Nil = True
  -- Functions are never equal
  -- TODO: maybe compare by reference in future?
  (PrimitiveFunc _) == (PrimitiveFunc _) = False
  (Function {}) == (Function {}) = False
  -- Everything else is never equal
  _ == _ = False

-- NOTE: Instead of deriving stock Show, we can implement our own. This will
-- print using showVal, so output will look like Scheme code.
-- Make LispVal an instance of Show for debugging
instance Show LispVal where
  show = T.unpack . showVal

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
  PrimitiveFunc _ -> "<primitive>"
  Function params _ _ ->
    "<procedure:(" <> T.intercalate " " params <> ")>"

type Env = Map Text LispVal

newtype EnvRef = EnvRef (IORef Env)

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
  deriving stock (Show)

-- | This is a type alias for operations that might fail
-- `Either SchemeError a` means the operation will either produce an error or a
-- value of type 'a'
type ThrowsError = Either SchemeError

-- | Type for IO operations that might fail
-- `ExceptT` is a monad transformer that adds error handling to IO
type IOThrowsError = ExceptT SchemeError IO
