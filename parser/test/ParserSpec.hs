{-# LANGUAGE ViewPatterns #-}

module ParserSpec where


import Test.Hspec
import Lib
import FrisbeeParser



parsesTo (parseText -> Right prog) expected = prog `shouldBe` expected
parsesTo (parseText -> Left err) expected = expectationFailure err


failsWith (parseText -> Left err) "" = err `shouldSatisfy` \s -> (length s) > 0  -- any error with msg supplied
failsWith (parseText -> Left err) s = err `shouldBe` s
failsWith (parseText -> Right prog) _ = 
  expectationFailure $ "Expected to fail, but parsed to\n" ++ (show prog)

spec :: Spec
spec = do
  describe "Import statements" $ do

    it "Simple import" $ do
        "from x import A;" `parsesTo` (Program [ImportDecl "x" ["A"]] [])

    it "Semicolon if required" $ do
        "from x import A" `failsWith` "Wrong program structure"
