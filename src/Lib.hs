module Lib where

import Text.Pretty.Simple (pPrint)

import Syntax
import Parser
import Evaluation
import Environment


parse :: String -> IO ([Statement])
parse line = do
  let res = parseToplevel line
  case res of
    Left err -> pPrint err >> return []
    Right ex -> return ex


startInterpreter :: (Maybe Env) -> [Statement] -> IO (Env)
startInterpreter Nothing es = startInterpreter (Just emptyEnv) es
startInterpreter (Just env) exprs = do
   (newEnv, _) <- runExpressions env False Nothing exprs
   return newEnv
