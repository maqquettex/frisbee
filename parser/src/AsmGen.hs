{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}


module AsmGen where

import Data.List
import Data.Map

import FrisbeeParser

passiveFirst :: ObjectDecl -> ObjectDecl -> Ordering
passiveFirst (PassiveDecl _ _ _)  _    = LT
passiveFirst active  _ | isMain active = GT  -- main as last declaration
passiveFirst _ _                       = GT

isMain :: ObjectDecl -> Bool
isMain (ActiveDecl "Main" _ _) = True
isMain _ = False


genVerify :: Bool -> String -> Either String ()
genVerify pred err = if pred then Right () else Left err

verifyMain :: [ObjectDecl] -> Either String ()
verifyMain decls = genVerify (any isMain decls)  "No Main provided!"

verifyRedefine :: [ObjectDecl] -> Either String ()
verifyRedefine decls =
    genVerify noRedefine "Redefine!"
    where 
        noRedefine = all (\x -> (length x) == 1) groupedDecls
        groupedDecls = group $ sort $ (getName <$> decls)
        getName (ActiveDecl n _ _)  = n
        getName (PassiveDecl n _ _) = n


preprocess :: Program -> Either String Program
preprocess prog@(Program imports decls) = do
    let sortedDecls = sortBy passiveFirst decls
    
    verifyMain sortedDecls
    verifyRedefine sortedDecls
    
    return $ Program imports sortedDecls




generate :: Program -> Either String [String]
generate prog@(Program _ decls) = do
    processed <- preprocess prog
    return ["One"]
