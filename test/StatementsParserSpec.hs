{-# LANGUAGE ViewPatterns #-}

module StatementsParserSpec where

import Data.Either
import Test.Hspec

import Parser
import Syntax


shouldParseTo (parseToplevel -> Left err) expected = error $ "Error occured! " ++ show err
shouldParseTo (parseToplevel -> Right exprs) expected = exprs `shouldBe` [expected]

shouldFail (parseToplevel -> Left err) = 1 `shouldBe` 1
shouldFail (parseToplevel -> Right exprs) = error $ "Error not occured, AST is " ++ show exprs

-- !!! WARNING !!!
-- Function body should always be empty to avoid statements check


spec :: Spec
spec = do
  describe "Active objects" $ do
    it "Active object definition" $ do
      let expr = Expression (BinaryOp Plus (Value $ Integer 1)
                                           (Value $ Integer 1))
      "active SomeName { 1+1 };" `shouldParseTo` (ActiveDef "SomeName" expr)
    