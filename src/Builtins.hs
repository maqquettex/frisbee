module Builtins where

import Syntax


proceedBuiltin :: BuiltinFunction -> [Value] -> IO (Value)

proceedBuiltin Print values = putStrLn (unwords (map valueToString values)) >> return Null

proceedBuiltin IsNull [Null]  = return (Boolean True)
proceedBuiltin IsNull [value] = return (Boolean False)
proceedBuiltin IsNull args    = error ("isnull expected single argument, but got " ++ show args)

proceedBuiltin Length [Array vs] = return (Integer (fromIntegral (length vs)))
proceedBuiltin Length [other]    = error ("Not array passed to length() function: " ++ show other)
proceedBuiltin Length args       = error ("length expected single argument, but got " ++ show args)
