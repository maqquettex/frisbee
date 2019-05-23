{-# LANGUAGE ViewPatterns #-}

module StatementParserSpec where
import Data.Either
import Test.Hspec

import Parser
import Syntax

shouldParseTo (parseToplevel -> Left err) expected = error $ "Error occured! " ++ show err
shouldParseTo (parseToplevel -> Right exprs) expected = exprs `shouldBe` [expected]

shouldFail (parseToplevel -> Left err) = 1 `shouldBe` 1
shouldFail (parseToplevel -> Right exprs) = error $ "Error not occured, AST is " ++ show exprs


spec :: Spec
spec = do
  describe "process" $ do
    it "1. Empty `return` is allowed" $ do
      let result = Function Nothing [] [(Return Nothing)]
      "function() {return;}" `shouldParseTo` (Expression result)

    it "2. `return` in `if` expression, if without braces" $ do
      let programText = "function () {\
                        \  if (true) return true;\
                        \  else return false;\
                        \}"
      let result = Function Nothing [] [(If (Value (Boolean True))
                                        [(Return $ Just (Value (Boolean True)))]
                                        (Just [(Return $ Just (Value (Boolean False)))]))]
      programText `shouldParseTo` (Expression result)

    it "3. `return` in `while` expression, while without braces" $ do
      let programText = "function () {\
                        \  while(false)\
                        \    return 1;\
                        \}"
      let result = Function Nothing [] [(While (Value (Boolean False))
                                         [(Return $ Just (Value (Integer 1)))])]
      programText `shouldParseTo` (Expression result)


    it "4. `while`, `if` and `function` expressions with blocks without braces" $ do
      let programText = "foo = function () {\
                        \  while(false)\
                        \    if(true)\
                        \      function() return true;\
                        \    else\
                        \      while (false)\
                        \        false;\
                        \};"
      let whileBody = [If (Value $ Boolean True)
                          [Expression (Function Nothing [] [(Return $ Just (Value $ Boolean True))])]
                           (Just [While (Value $ Boolean False)
                                        [(Expression (Value $ Boolean False))]])]
      let result = Assign
                     "foo"
                     (Function Nothing
                        []
                        [(While (Value $ Boolean False) whileBody)])

      programText `shouldParseTo` result

    it "5. Chained calls" $ do
      let programText = "(x()(1)) (function() {})(true);"
      let firstCall = Call (Variable "x") []
      let secondCall = Call firstCall [(Value $ Integer 1)]
      let thirdCall = Call secondCall [(Function Nothing [] [])]
      let lastCall = Call thirdCall [(Value $ Boolean True)]
      programText `shouldParseTo` (Expression lastCall)


    it "6. `break` and `continue` allowed in if block" $ do
      let programText = "while (true)\
                        \  if (1) break;\
                        \  else continue;"

      let result = While (Value $ Boolean True)
                         [(If (Value $ Integer 1)
                             [Break]
                             (Just [Continue]))]
      programText `shouldParseTo` (result)

    it "7. `break` not allowed in function inside while" $ do
      let programText = "while (true) function() break;"
      shouldFail programText

    it "8. `continue` not allowed in function inside while" $ do
      let programText = "while (true) function() break;"
      shouldFail programText

    it "9. 'return` not allowed outside of function body" $ do
      shouldFail "return;"

    it "10. 'return` allowed inside of function body" $ do
      let withBody body = (Expression (Function Nothing [] body))

      "function () {return somevar;}" `shouldParseTo` (withBody
                                                            [Return
                                                              (Just $ Variable "somevar")])

    it "11. Nested control-flow expressions, `return`, `break` and `continue` allowed" $ do
      let programText = "function() {\
                        \  while (true) {\
                        \    if (true) {\
                        \      return;\
                        \      break;\
                        \      continue;\
                        \    }\
                        \  }\
                        \}"

      let result = Function Nothing [] [(While (Value $ Boolean True)
                                           [If (Value $ Boolean True)
                                               [(Return Nothing), Break, Continue]
                                               Nothing])]
      programText `shouldParseTo` (Expression result)

    it "12. `if`, `function` and `while` does not require semicolon" $ do
      shouldFail "function () {};"
      shouldFail "if (1) {};"
      shouldFail "if (1) {} else {};"
      shouldFail "while (1) {};"

      "function () {}" `shouldParseTo` (Expression (Function Nothing [] []))
      "if (1) {}" `shouldParseTo` (If (Value $ Integer 1) [] Nothing)
      "if (1) {} else {}" `shouldParseTo` (If (Value $ Integer 1) [] (Just []))
      "while (1) {}" `shouldParseTo` (While (Value $ Integer 1) [])
