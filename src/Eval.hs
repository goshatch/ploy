module Eval (eval, primitiveEnv) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Environment
import SchemeError
import Types (EnvRef, LispVal (..), showVal)

-- one-liner for testing:
-- let testPrimitive code = do { env <- primitiveEnv; either (putStrLn . ("Parse error: " ++) . show) (\expr -> runIOThrows (eval env expr) >>= print) (parseSingle (T.pack code)) }

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
  List [Atom "define", Atom var, expr] -> do
    value <- eval envRef expr
    liftIO $ defineVar envRef var value
  List [Atom "set!", Atom var, expr] -> do
    value <- eval envRef expr
    result <- setVar envRef var value
    liftThrows result

  -- \| `let` will evaluate all bindings, then create a new environment with the
  -- \| evaluated bindings, then evaluate the body in the new environment.
  List [Atom "let", List bindings, body] -> do
    evaledBindings <- mapM evalBinding bindings
    newEnv <- liftIO $ bindVars envRef evaledBindings
    eval newEnv body
    where
      evalBinding = \case
        List [Atom var, expr] -> do
          val <- eval envRef expr
          return (var, val)
        bad -> throwError $ BadSpecialForm "Bad let binding" bad
  List [Atom "lambda", List params, body] -> do
    paramNames <- mapM extractVarName params
    return $ Function paramNames body envRef
    where
      extractVarName = \case
        Atom name -> return name
        bad -> throwError $ TypeMismatch "atom" bad

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
  [ -- Arithmetic
    ("+", numericBinop (+)),
    ("-", numericBinop (-)),
    ("*", numericBinop (*)),
    ("/", integerDivop),
    ("mod", numericBinop mod),
    ("quotient", numericBinop quot),
    ("remainder", numericBinop rem),
    -- Comparison
    ("=", numBoolBinop (==)),
    ("<", numBoolBinop (<)),
    (">", numBoolBinop (>)),
    ("<=", numBoolBinop (<=)),
    (">=", numBoolBinop (>=)),
    -- List operations
    ("car", car),
    ("cdr", cdr),
    ("cons", cons),
    -- Type predicates
    ("null?", isNull),
    ("list?", isList),
    ("number?", isNumber),
    ("string?", isString),
    ("symbol?", isSymbol),
    ("boolean?", isBoolean),
    -- Equality
    ("eq?", eqv),
    ("eqv?", eqv),
    ("equal?", equal)
  ]

numericBinop ::
  (Integer -> Integer -> Integer) ->
  [LispVal] ->
  IOThrowsError LispVal
numericBinop op = \case
  [] -> throwError $ NumArgs 2 []
  [_] -> throwError $ NumArgs 2 []
  args -> do
    nums <- mapM unpackNum args
    return $ Number $ foldl1 op nums

-- | Integer division with div-by-zero check
integerDivop :: [LispVal] -> IOThrowsError LispVal
integerDivop = \case
  [a, Number 0] -> throwError DivisionByZero
  [a, b] -> numericBinop div [a, b]
  args -> throwError $ NumArgs 2 args

-- | Helper: lift numeric comparison
numBoolBinop ::
  (Integer -> Integer -> Bool) ->
  [LispVal] ->
  IOThrowsError LispVal
numBoolBinop op = \case
  [Number a, Number b] -> return $ Bool $ op a b
  [a, b] ->
    throwError $
      TypeMismatch "number" $
        if not (isNum a) then a else b
  args -> throwError $ NumArgs 2 args
  where
    isNum (Number _) = True
    isNum _ = False

-- | Extract number from LispVal
unpackNum :: LispVal -> IOThrowsError Integer
unpackNum = \case
  Number n -> return n
  val -> throwError $ TypeMismatch "number" val

-- List operations
car :: [LispVal] -> IOThrowsError LispVal
car = \case
  [List (x : xs)] -> return x
  [List []] -> throwError $ Default "Empty list"
  [DottedList (x : xs) _] -> return x
  [arg] -> throwError $ TypeMismatch "pair" arg
  args -> throwError $ NumArgs 1 args

cdr :: [LispVal] -> IOThrowsError LispVal
cdr = \case
  [List (_ : xs)] -> return xs
  [List []] -> throwError $ Default "Empty list"
  [DottedList [_] x] -> return x
  [DottedList (_ : xs) x] -> return $ DottedList xs x
  [arg] -> throwError $ TypeMismatch "pair" arg
  args -> throwError $ NumArgs 1 args

cons :: [LispVal] -> IOThrowsError LispVal
cons = \case
  [x, Nil] -> return $ List [x]
  [x, List xs] -> return $ List (x : xs)
  [x, DottedList xs xlast] -> return $ DottedList (x : xs) xlast
  [x, y] -> return $ DottedList [x] y
  args -> throwError $ NumArgs 2 args

isNull :: [LispVal] -> IOThrowsError LispVal
isNull = \case
  [Nil] -> return $ Bool True
  [List []] -> return $ Bool True
  [_] -> return $ Bool False
  args -> throwError $ NumArgs 1 args

isList :: [LispVal] -> IOThrowsError LispVal
isList = \case
  [List _] -> return $ Bool True
  [Nil] -> return $ Bool True
  [_] -> return $ Bool False
  args -> throwError $ NumArgs 1 args

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

isSymbol :: [LispVal] -> IOThrowsError LispVal
isSymbol = \case
  [Atom _] -> return $ Bool True
  [_] -> return $ Bool False
  args -> throwError $ NumArgs 1 args

isBoolean :: [LispVal] -> IOThrowsError LispVal
isBoolean = \case
  [Bool _] -> return $ Bool True
  [_] -> return $ Bool False
  args -> throwError $ NumArgs 1 args

-- Equality operations
eqv :: [LispVal] -> IOThrowsError LispVal
eqv = \case
  [Bool a, Bool b] -> return $ Bool $ a == b
  [Number a, Number b] -> return $ Bool $ a == b
  [String a, String b] -> return $ Bool $ a == b
  [Atom a, Atom b] -> return $ Bool $ a == b
  [Nil, Nil] -> return $ Bool True
  [List [], List []] -> return $ Bool True
  [_, _] -> return $ Bool False
  args -> throwError $ NumArgs 2 args

equal :: [LispVal] -> IOThrowsError LispVal
equal = \case
  [arg1, arg2] -> do
    -- first, try eqv?
    primitiveEquals <- eqv [arg1, arg2]
    case primitiveEquals of
      Bool True -> return $ Bool True
      _other -> case (arg1, arg2) of
        (List xs, List ys) ->
          if length xs /= length ys
          then return $ Bool False
          else do
            results <- zipWithM (\x y -> equal [x, y]) xs ys
            return $ Bool $ all (== Bool True) results
        _other -> return $ Bool False
  args -> throwError $ NumArgs 2 args

-- | Create initial environment with primitives
primitiveEnv :: IO EnvRef
primitiveEnv =
  makeEnvRef $
    Map.fromList
      [(name, PrimitiveFunc func) | (name, func) <- primitives]
