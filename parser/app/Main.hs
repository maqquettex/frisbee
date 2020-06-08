module Main where

import System.Environment
import Lib
import AsmGen

main :: IO ()
main = do
    args <- getArgs
    let default_file = "../examples/test.frisbee"

    case args of
        [] -> readFile default_file >>= printTree
        ["asm"] -> readFile default_file >>= printAsm
        
        ["stdin"] -> getContents >>= printTree
        ["stdin", "asm"] -> getContents >>= printAsm

        [filename] -> readFile filename >>= printTree
        [filename, "asm"] -> readFile filename >>= printAsm
        
        _ -> print "IDK what to do really" 
