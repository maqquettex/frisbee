{-# LANGUAGE ViewPatterns #-}

module Lib
    ( parseText,
      printTree,
      printAsm
    ) where

import FrisbeeParser
import Tokens
import AsmGen

import Text.Pretty.Simple (pPrintNoColor)

parseText :: String -> Either String Program
parseText s = alexScanTokensCustom s >>= astparser 

parseTextAndGen :: String -> Either String [String]
parseTextAndGen s = (parseText s) >>= generate


printTree :: String -> IO ()
printTree (parseText -> Right prog) = pPrintNoColor prog
printTree (parseText -> Left err)   = print err


-- printAsm:: String -> IO()
-- printAsm (parseTextAndGen -> Right strs) = pPrintNoColor strs
-- printAsm (parseTextAndGen -> Left err)   = print err

printAsm:: String -> IO()
printAsm (\s -> (parseText s) >>= preprocess -> Right prog) = pPrintNoColor prog
printAsm (\s -> (parseText s) >>= preprocess -> Left err)   = print err
