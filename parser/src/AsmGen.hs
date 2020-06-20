{-# LANGUAGE ViewPatterns #-}

module AsmGen where

import Data.List

import FrisbeeParser


preprocess :: Program -> Either String Program
preprocess (Program imports decls) = 
    Right (Program imports (sortBy passiveFirst decls))
    where 
        passiveFirst (PassiveDecl _ _ _)  _ = LT
        passiveFirst (ActiveDecl _ _ _)   _ = GT


generate :: Program -> Either String [String]
generate prog@(Program _ decls) = do
    processed <- preprocess prog
    return ["One"]
