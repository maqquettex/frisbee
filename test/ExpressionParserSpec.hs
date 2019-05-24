{-# LANGUAGE ViewPatterns #-}

module ExpressionParserSpec where

import Data.Either
import Data.List
import qualified Text.Parsec as P
import Test.Hspec

import Parser
import Syntax

expressionParser :: String -> Either P.ParseError Expression
expressionParser input = P.parse (contents expression) "stdin" input

shouldParseTo (expressionParser -> Left err) expected = error $ "Error occured! " ++ show err
shouldParseTo (expressionParser -> Right expr) expected = expr `shouldBe` expected

shouldFail (parseToplevel -> Left err) = 1 `shouldBe` 1
shouldFail (parseToplevel -> Right stmts) = error $ "Error not occured, AST is " ++ show stmts

-- !!! WARNING !!!
-- Function body should always be empty to avoid statements check


spec :: Spec
spec = do
  describe "process" $ do

    it "Call could be right operand of operator" $ do
      "1 + some()" `shouldParseTo` (BinaryOp Plus
                                             (Value $ Integer 1)
                                             (Call (Variable "some") []))
    it "Call could be left operand of operator" $ do
      "some() + 1" `shouldParseTo` (BinaryOp Plus
                                             (Call (Variable "some") [])
                                             (Value $ Integer 1))

    it "Call could be left operand of unary operators" $ do
      "not foo()" `shouldParseTo` (UnaryOp Not (Call (Variable "foo") []))
      "- foo()" `shouldParseTo` (UnaryOp Negate (Call (Variable "foo") []))

    let array = ArrayDeclare :: [Expression] -> Expression
    let subscr expr num = Subscript expr (Value (Integer num))
   
    it "Array with call and as call argument" $ do
      "[x(), y([4.2, null])]" `shouldParseTo` array [ (Call (Variable "x") [])
                                                    , (Call (Variable "y")
                                                            [(array [ (Value $ Double 4.2)
                                                                    , (Value Null)])])]

    it "Array as operand" $ do
      "[1 + 2]" `shouldParseTo` array [BinaryOp Plus (Value $ Integer 1)
                                                     (Value $ Integer 2)]

      "[] + [2]" `shouldParseTo` (BinaryOp Plus (array []) (array [(Value $ Integer 2)]))
      "x() + []" `shouldParseTo` (BinaryOp Plus (Call (Variable "x") [])
                                                (array []))
     

    it "Array subscription" $ do
      "[1, 2][0]" `shouldParseTo` (subscr (array [ (Value $ Integer 1)
                                                    , (Value $ Integer 2)]) 0)
      "x[0]" `shouldParseTo` (subscr (Variable "x") 0)

      shouldFail "x[1, 2]"
      shouldFail "x[1.0]"
      shouldFail "x[1]"
      shouldFail "x[\"1\"]"

    it "Chained array subscriptions and calls" $ do
      "x[0]()" `shouldParseTo` (Call (subscr (Variable "x") 0) [])

      "x()[1]()" `shouldParseTo` (Call (subscr (Call (Variable "x") []) 1) [])
      "(x()[1])()" `shouldParseTo` (Call (subscr (Call (Variable "x") []) 1) [])
      "(x())[1]()" `shouldParseTo` (Call (subscr (Call (Variable "x") []) 1) [])

      "(x()[1]) [5]" `shouldParseTo` (subscr (subscr (Call (Variable "x") []) 1) 5)
      "(x())[1][5]" `shouldParseTo` (subscr (subscr (Call (Variable "x") []) 1) 5)

    it "REALLY A LOT of chained subscriprions and calls" $ do
      let sub expr = (subscr expr 0)
      let call expr = (Call expr [])
      let chain = sub . call . sub . call . sub . call . sub

      "x[0]()[0]()[0]()[0]" `shouldParseTo` (chain (Variable "x"))
      "(x[0]()[0])()[0]()[0]" `shouldParseTo` (chain (Variable "x"))
      "(x[0]()[0]())[0]()[0]" `shouldParseTo` (chain (Variable "x"))

