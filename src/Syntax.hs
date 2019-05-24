module Syntax where

import qualified Data.Map.Strict as Map
import Data.List

data Value = Integer Integer
           | Double Double
           | Boolean Bool
           | String String
           | Null

           | Array [Value]  -- result of ArrayDeclare expr
           deriving (Show, Ord, Eq)

data Expression = Value Value
                | Variable String  -- name of variable
                | Call Expression [Expression]  -- Callable and arguments
                | ArrayDeclare [Expression]

                -- Operators and array subscription
                | BinaryOp BinaryOp Expression Expression
                | UnaryOp UnaryOp Expression
                | Subscript Expression Expression

                deriving (Show, Ord, Eq)


data Statement = Expression Expression
               | Assign String Expression
               | While Expression [Statement]
               | If Expression [Statement] (Maybe [Statement])

               -- While loop body only
               | Continue
               | Break
               deriving (Show, Ord, Eq)


data BinaryOp = Plus
              | Minus
              | Multiply
              | Divide
              
              | Less
              | LessOrEqual
              | Greater
              | GreaterOrEqual
              | Equal
              | NotEqual
              
              | And
              | Or
              deriving (Show, Ord, Eq)

data UnaryOp = Not
             | Negate
             deriving (Show, Ord, Eq)


valueToString :: Value -> String
valueToString value =
  case value of
    Integer i -> (show i)
    Double d  -> (show d)

    Boolean True -> "true"
    Boolean False -> "false"

    String s -> s

    Null -> "null"

    Array values -> ("[" ++ intercalate ", " (map valueToString values) ++ "]")
