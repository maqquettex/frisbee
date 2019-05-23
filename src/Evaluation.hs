{-# LANGUAGE ViewPatterns #-}

module Evaluation where

import Data.List (foldl')
import Control.Monad


import Lexer
import Syntax
import Environment
import Operators
import Builtins

-- Evaluate Expressions

evalExpression :: Expression -> Env -> IO (Value)

evalExpression (Value value) _ = return value

evalExpression (Variable var) env =
  case env `lookupEnv` var of
    Just v  -> return v
    Nothing -> error ("Variable " ++ var ++ " not found in scope!")


evalExpression (Function optName args body) env =
  case optName of
    Nothing     -> return (Closure optName env args body)
    (Just name) ->
      let closure = Closure optName (addToEnv name closure env) args body
      in return closure

evalExpression (BinaryOp operator leftExpr rightExpr) env = do
  leftOperand  <- (evalExpression leftExpr env)
  rightOperand <- (evalExpression rightExpr env)
  return (evalBinaryOp operator leftOperand rightOperand)

evalExpression (UnaryOp operator expr) env = do
  operand <- (evalExpression expr env)
  return (evalUnaryOp operator operand)

evalExpression (ArrayDeclare exprs) env = Array <$> (mapM (`evalExpression` env) exprs)

-- TODO: Uncaught exceptions could happen
evalExpression (Subscript expr indexExpr) env = do
  (Integer i) <- (evalExpression indexExpr env)
  (Array values) <- (evalExpression expr env)
  return $ values !! (fromIntegral i)
    

evalExpression (Call expr argExprs) env = do
  callable <- (evalExpression expr env)

  case callable of
    BuiltinFunction builtin ->
      (mapM (`evalExpression` env) argExprs) >>= proceedBuiltin builtin

    Closure n closureEnv args body -> do
      argValues <- (mapM (`evalExpression` env) argExprs)

      if length args /= length argValues
      then error $ "Wrong number of arguments passed to " ++ show n ++ " : " ++ show args
      else do
        let newEnv = unionEnv (envFromList $ zip args argValues) closureEnv
        (_, Just callResult) <- runExpressions newEnv True Nothing body
        return callResult

    _ -> error $ "Non-function is called: " ++ show callable


-- Execute statements
-- TODO: rewrite lower code as monadic

runExpressions :: Env             -- initial env
                  -> Bool         -- whether inside of function call
                  -> Maybe Statement   -- possible current while loop
                  -> [Statement]       -- statements to run
                  -> IO (Env, Maybe Value)  -- new env, result of runExpression
runExpressions env currCall currWhile stmts = do

  case stmts of
    [] ->
      case currWhile of
        Nothing -> return (env, if currCall then (Just Null) else Nothing)
        Just (While cond whileExprs) -> do
          condValue <- evalExpression cond env
          case condValue of
            (Boolean True)  -> runExpressions env currCall currWhile whileExprs
            (Boolean False) -> return (env, Nothing)

    (Expression e):exprs' -> do
      _ <- (evalExpression e env)
      runExpressions env currCall currWhile exprs'

    (Assign var expr):exprs' -> do
      value  <- (evalExpression expr env)
      runExpressions (addToEnv var value env) currCall currWhile exprs'

    (Return Nothing):_     -> return (env, Just Null)
    (Return (Just expr)):_  -> do
      value  <- (evalExpression expr env)
      return (env, Just value)

    (Continue:_) -> runExpressions env currCall currWhile []
    (Break:_)    -> return (env, Nothing)

    (If cond trueExprs falseExprs):exprs' -> do
      condValue <- evalExpression cond env
      let continueRun = runExpressions env currCall currWhile
      case (condValue, falseExprs) of
        (Boolean True, _)                -> continueRun (trueExprs  ++ exprs')
        (Boolean False, Just falseExprs) -> continueRun (falseExprs ++ exprs')
        (Boolean False, Nothing)         -> continueRun exprs'
        _ -> error ("Condition of if-expression is not boolean but " ++ show condValue)

    (newWhile@(While cond whileExprs):exprs') -> do
      condValue <- evalExpression cond env
      case condValue of
        (Boolean False) -> runExpressions env currCall currWhile exprs'
        (Boolean True) -> do
            (newEnv, _) <- runExpressions env currCall (Just newWhile) whileExprs
            runExpressions newEnv currCall currWhile exprs'
        _ -> error $ "Condition of while-expression is not boolean but " ++ show condValue
