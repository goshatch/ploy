-- |

module Eval where

import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T

import Environment
import SchemeError
import SchemeTypes

primitiveEnv :: IO EnvRef
primitiveEnv = makeEnvRef $ Map.fromList
  [(name, PrimitiveFunc func) | (name, func) <- primitives]
