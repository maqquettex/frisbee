module Lib
    ( printTree,
      printAsm
    ) where

import Frisbee
import Tokens

import Text.Pretty.Simple (pPrint, pPrintNoColor)

parseText = frisbee . alexScanTokens2

printTree :: String -> IO ()
printTree inStr = do
    pPrintNoColor $ parseText inStr


printAsm:: String -> IO()
printAsm s = do
    let x = parseText s
    print "Some day here will be ASM!"


