module Main where

import Lib
import Environment
-- import Evaluation (Env)

import Control.Monad.Trans
import System.Environment
import System.Console.Haskeline

import Text.Pretty.Simple (pPrint)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["repl"]       -> runREPL
    ["ast"]        -> runASTDebug
    ["ast", fname] -> parseFileAST fname
    [fname]        -> parseFile fname >> return ()
    _              -> help

help :: IO ()
help = putStr $ unlines
    [ "Usage: bauble [cmd | pathToFile]"
    , ""
    , "Commands:"
    , "  repl      start REPL"
    , "  ast       start REPL for AST debugging"
    ]


runASTDebug :: IO ()
runASTDebug = runInputT defaultSettings loop
  where
  loop = do
    minput <- getInputLine "ready> "
    case minput of
      Nothing    -> outputStrLn "Goodbye."
      Just input -> (liftIO $ (parse input) >>=
                      (\ast -> (pPrint ast >> return ()))) >> loop

parseFileAST file = do
  program  <- readFile file
  exprs    <- parse program
  putStrLn ("----- AST of " ++ file ++ " -----")
  pPrint exprs
  putStrLn ("---------- END ----------")

        

processLine :: String -> Maybe Env -> IO(Maybe Env)
processLine line env = do
  exprs  <- parse line
  newEnv <- startInterpreter env exprs
  return (Just newEnv)


runREPL :: IO ()
runREPL = runInputT defaultSettings (loop Nothing)
  where
  loop env = do
    minput <- getInputLine "ready> "
    case minput of
      Nothing    -> outputStrLn "Goodbye."
      Just input -> (liftIO $ processLine input env) >>= loop
        

parseFile file = do
  program  <- readFile file
  exprs    <- parse program
  putStrLn ("----- Running " ++ file ++ " -----")
  _        <- startInterpreter Nothing exprs
  putStrLn ("---------- END ----------")

