module Lib
    ( parseText,
      printTree,
      printAsm
    ) where

import FrisbeeParser
import Tokens

import Text.Pretty.Simple (pPrint, pPrintNoColor)

parseText :: String -> Either String Program
parseText s = case alexScanTokensCustom s of 
    Right tokens -> 
        case astparser tokens of
            Right prog -> Right prog
            Left err -> Left err
    Left err -> Left err

printTree :: String -> IO ()
printTree inStr = do
    pPrintNoColor $ parseText inStr


printAsm:: String -> IO()
printAsm s = do
    let x = parseText s
    print "Some day here will be ASM!"


