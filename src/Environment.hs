{-# LANGUAGE ImportQualifiedPost #-}

module Environment
  ( emptyEnv,
    makeEnvRef,
    getVar,
    setVar,
    defineVar,
    bindVars,
  )
where

import Control.Monad.IO.Class
import Data.IORef
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Types (Env, EnvRef (..), LispVal (..), SchemeError (..), ThrowsError)

-- | Create an empty environment
emptyEnv :: Env
emptyEnv = Map.empty

-- | Create a mutable environment reference
makeEnvRef :: Env -> IO EnvRef
makeEnvRef env = EnvRef <$> newIORef env

-- | Look up a variable
getVar :: (MonadIO m) => EnvRef -> Text -> m (ThrowsError LispVal)
getVar (EnvRef ref) var = do
  env <- liftIO $ readIORef ref
  return $ case Map.lookup var env of
    Nothing -> Left $ UnboundVar "Getting an unbound variable" var
    Just val -> Right val

-- | Set an existing variable
setVar :: (MonadIO m) => EnvRef -> Text -> LispVal -> m (ThrowsError LispVal)
setVar envRef@(EnvRef ref) var value = do
  varExists <- liftIO $ Map.member var <$> readIORef ref
  if varExists
    then liftIO $ do
      modifyIORef ref (Map.insert var value)
      return $ Right value
    else return $ Left $ UnboundVar "Setting an unbound variable" var

-- | Define a new variable, or update an existing var
defineVar :: (MonadIO m) => EnvRef -> Text -> LispVal -> m LispVal
defineVar (EnvRef ref) var value = liftIO $ do
  modifyIORef ref (Map.insert var value)
  return value

-- | Bind multiple vars at once
bindVars :: EnvRef -> [(Text, LispVal)] -> IO EnvRef
bindVars (EnvRef ref) bindings = do
  env <- readIORef ref
  newRef <- newIORef $ Map.union (Map.fromList bindings) env
  return $ EnvRef newRef
