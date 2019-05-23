module Environment where

import qualified Data.Map.Strict as Map
import Syntax (Value)
import qualified Parser as P


-- Encapsulate Env internal structure
type Env = Map.Map String Value

emptyEnv :: Env
emptyEnv = Map.fromList []

lookupEnv :: Env -> String -> Maybe Value
lookupEnv = (Map.!?)

addToEnv :: String -> Value -> Env -> Env
addToEnv = Map.insert

unionEnv :: Env -> Env -> Env
unionEnv = Map.union

envFromList :: [(String, Value)] -> Env
envFromList = Map.fromList
