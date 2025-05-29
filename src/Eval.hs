module Eval (eval, primitiveEnv) where

import Control.Monad (when)
import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Environment
import SchemeError
import Types (EnvRef, LispVal (..), showVal)

eval :: EnvRef -> LispVal -> IOThrowsError LispVal
eval envRef = \case
  -- Self-evaluating forms
  val@(String _) -> return val
  val@(Number _) -> return val
  val@(Bool _) -> return val
  Nil -> return Nil
  -- Variable reference
  Atom var -> do
    result <- getVar envRef var
    liftThrows result

  -- Special forms
  List [Atom "quote", val] ->
    return val
  List [Atom "if", predicate, conseq, alt] -> do
    result <- eval envRef predicate
    case result of
      Bool False -> eval envRef alt
      -- we are pattern-matching on `_other` here because in Scheme, anything
      -- that is not explicityly #f is considered truthy.
      _other -> eval envRef conseq

  -- TODO: Various other Atoms

  -- Function application
  List (func : args) -> do
    f <- eval envRef func
    argVals <- mapM (eval envRef) args
    apply f argVals

  -- Error cases
  bad -> throwError $ BadSpecialForm "Cannot evaluate" bad

-- | Apply a function to arguments
apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply = \case
  PrimitiveFunc f -> f -- Primitives handle their own args
  Function params body closure -> \args -> do
    when (length params /= length args) $
      throwError $
        NumArgs (fromIntegral $ length params) args

    -- Create new environment with parameters bound to arguments
    newEnv <- liftIO $ bindVars closure (zip params args)

    -- Evaluate the body in the new environment
    eval newEnv body
  notFunc -> \args ->
    throwError $ NotFunction "Attempting to call non-function" (showVal notFunc)

-- | Primitive functions
primitives :: [(Text, [LispVal] -> IOThrowsError LispVal)]
primitives =
  [ -- Type predicates
    ("number?", isNumber),
    ("string?", isString)
  ]

isNumber :: [LispVal] -> IOThrowsError LispVal
isNumber = \case
  [Number _] -> return $ Bool True
  [_] -> return $ Bool False
  args -> throwError $ NumArgs 1 args

isString :: [LispVal] -> IOThrowsError LispVal
isString = \case
  [String _] -> return $ Bool True
  [_] -> return $ Bool False
  args -> throwError $ NumArgs 1 args

-- | Create initial environment with primitives
primitiveEnv :: IO EnvRef
primitiveEnv =
  makeEnvRef $
    Map.fromList
      [(name, PrimitiveFunc func) | (name, func) <- primitives]
