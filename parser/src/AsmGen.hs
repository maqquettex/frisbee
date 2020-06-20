module AsmGen where

import FrisbeeParser


generate :: Program -> [String]
generate (Program _ _) = ["One", "two"]
    
