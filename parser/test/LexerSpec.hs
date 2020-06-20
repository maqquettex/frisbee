{-# LANGUAGE ViewPatterns #-}

module LexerSpec where


import Test.Hspec
import Tokens



allGood = shouldBe 1 1

spec :: Spec
spec = do
  describe "Lexer check" $ do

    it "Simple tokens structure" $ do
      let tokens = alexScanTokensCustom "from x import A"
      case tokens of 
        Right [TFrom _, TIdent _ "x", TImport _, TTypeIdent _ "A"] -> allGood
        Right [] -> expectationFailure $ "Unexpected: no tokens produced " 
        Right pr -> pr `shouldBe` []  -- compare to empty for pretty print
        Left err -> expectationFailure $ "Unexpected error: " ++ err
      

    it "Basic error format" $ do
      (alexScanTokensCustom "from @x import A") `shouldBe` (Left "Line 1: lexical error")
    
    it "Error on not-first line" $ do
      (alexScanTokensCustom "from x import A\n passive Hello@{};") `shouldBe` (Left "Line 2: lexical error")
     