module Operators where

import Syntax
import Data.Ratio


-- HELPER FUNCTIONS
numToRat (Integer int) = Rational (toRational int)
numToRat (Rational r)  = Rational r
numToRat (Double d)    = error ("Double (" ++ show d ++ ") cannot be converted to rational!")

numToDbl (Integer int) = Double (fromInteger int)
numToDbl (Rational r)  = Double (fromRational r)
numToDbl (Double d)    = Double d

-- Reduces rational to integer if possible
reduceRat :: Rational -> Value
reduceRat r  = if (denominator r == 1)
               then (Integer $ numerator r)
               else (Rational r)

-- BINARY OPERATORS

evalBinaryOp :: BinaryOp -> Value -> Value -> Value

-- Math operators
evalBinaryOp Plus     (Integer l) (Integer r) = Integer (l + r)
evalBinaryOp Minus    (Integer l) (Integer r) = Integer (l - r)
evalBinaryOp Divide   (Integer l) (Integer r) = reduceRat ((toRational l) / (toRational r))
evalBinaryOp Multiply (Integer l) (Integer r) = Integer (l * r)

evalBinaryOp Plus     (Rational l) (Rational r) = reduceRat (l + r)
evalBinaryOp Minus    (Rational l) (Rational r) = reduceRat (l - r)
evalBinaryOp Divide   (Rational l) (Rational r) = reduceRat (l / r)
evalBinaryOp Multiply (Rational l) (Rational r) = reduceRat (l * r)

evalBinaryOp Plus     (Double l) (Double r) = Double (l + r)
evalBinaryOp Minus    (Double l) (Double r) = Double (l - r)
evalBinaryOp Divide   (Double l) (Double r) = Double (l / r)
evalBinaryOp Multiply (Double l) (Double r) = Double (l * r)

-- Always rise number type in the following order:
-- Integer -> Rational -> Double
-- Examples:
--   * Integer + Rational = Rational
--   * Rational + Double = Double
evalBinaryOp op l              r@(Double _)   = evalBinaryOp op (numToDbl l) r
evalBinaryOp op l@(Double _)   r              = evalBinaryOp op l (numToDbl r)
evalBinaryOp op l              r@(Rational _) = evalBinaryOp op (numToRat l) r
evalBinaryOp op l@(Rational _) r              = evalBinaryOp op l (numToRat r)

evalBinaryOp Plus (Array xs) (Array ys)   = Array (xs ++ ys)

-- Compare operators
evalBinaryOp Less     (Integer l) (Integer r) = Boolean (l < r)
evalBinaryOp Greater  (Integer l) (Integer r) = Boolean (l > r)
evalBinaryOp Equal    (Integer l) (Integer r) = Boolean (l == r)

evalBinaryOp Less     (Rational l) (Rational r) = Boolean (l < r)
evalBinaryOp Greater  (Rational l) (Rational r) = Boolean (l > r)
evalBinaryOp Equal    (Rational l) (Rational r) = Boolean (l == r)

evalBinaryOp Less     (Double l) (Double r) = Boolean (l < r)
evalBinaryOp Greater  (Double l) (Double r) = Boolean (l > r)
evalBinaryOp Equal    (Double l) (Double r) = Boolean (l == r)

-- Rise up too, as math operators do
evalBinaryOp op l              r@(Double _)   = evalBinaryOp op (numToDbl l) r
evalBinaryOp op l@(Double _)   r              = evalBinaryOp op l (numToDbl r)
evalBinaryOp op l              r@(Rational _) = evalBinaryOp op (numToRat l) r
evalBinaryOp op l@(Rational _) r              = evalBinaryOp op l (numToRat r)

evalBinaryOp LessOrEqual    l r = evalBinaryOp Or
                                               (evalBinaryOp Less l r)
                                               (evalBinaryOp Equal l r)

evalBinaryOp GreaterOrEqual l r = evalBinaryOp Or
                                               (evalBinaryOp Greater l r)
                                               (evalBinaryOp Equal l r)

evalBinaryOp NotEqual       l r = evalUnaryOp Not (evalBinaryOp Equal l r)

evalBinaryOp Equal (Boolean l)         (Boolean r)         = Boolean (l == r)
evalBinaryOp Equal (String l)          (String r)          = Boolean (l == r)
evalBinaryOp Equal Null                Null                = Boolean True
evalBinaryOp Equal (BuiltinFunction l) (BuiltinFunction r) = Boolean (l == r)
evalBinaryOp Equal (Array lv)          (Array rv)          = Boolean (lv == rv)
evalBinaryOp Equal _ _                                     = Boolean False

-- Boolean operators
evalBinaryOp And (Boolean b1) (Boolean b2) = Boolean (b1 && b2)
evalBinaryOp Or  (Boolean b1) (Boolean b2) = Boolean (b1 || b2)

evalBinaryOp op l r = error $ "evalBinaryOp called with operator "
                               ++ show op
                               ++ " and operands "
                               ++ show l
                               ++ " and "
                               ++ show r


evalUnaryOp :: UnaryOp -> Value -> Value

evalUnaryOp Not    (Boolean b) = Boolean (not b)

evalUnaryOp Negate (Integer  i) = Integer  (-i)
evalUnaryOp Negate (Rational r) = Rational (-r)
evalUnaryOp Negate (Double   d) = Double   (-d)

evalUnaryOp op v = error $ "Operator "
                           ++ show op
                           ++ " called with unappropriate operand "
                           ++ show v