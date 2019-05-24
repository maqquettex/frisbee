{-# LANGUAGE ViewPatterns #-}

module Parser where

import Text.Parsec
import qualified Text.Parsec.Error as Error
import Text.Parsec.String (Parser)
import Data.List (foldl')

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tk

import Lexer
import Syntax

trySeveral :: [Parser a] -> Parser a
trySeveral []           = error "trySeveral called with zero arguments!"
trySeveral (first:rest) = foldl (\fst -> \snd -> fst <|> try snd)
                                (try first) rest


-- ##########################
-- ### ATOMIC EXPRESSIONS ###
-- ##########################

-- Atomic expression is one that cannot be split up to more ones
-- Example: 1, 1.1, true, false, null, foo (values and identifiers)

-- Numbers
integerExpr = (Value . Integer) <$> integer
doubleExpr  = (Value . Double)  <$> float

-- Booleans
trueExpr =  reserved "true" >>  (return $ Value $ Boolean True)
falseExpr = reserved "false" >> (return $ Value $ Boolean False)

-- Null value
nullExpr = reserved "null" >> (return $ Value Null)

-- Strings
stringExpr = (Value . String) <$> stringLiteral

-- Variables
variableExpr = Variable <$> identifier

valueExpr = trySeveral [ doubleExpr
                       , integerExpr
                       , trueExpr
                       , falseExpr
                       , nullExpr
                       , stringExpr ]

atomicExpr =  trySeveral [ valueExpr, variableExpr ]

-- ##########################
-- ### SIMPLE EXPRESSIONS ###
-- ##########################

arrayExpr = ArrayDeclare <$> (brackets $ commaSep expression)

-- OPERATORS TABLE

binary s f assoc = Ex.Infix  (reservedOp s >> return (BinaryOp f)) assoc
unary  s f       = Ex.Prefix (reservedOp s >> return (UnaryOp f))

numberOperatorsTable = [ [ unary  "-"  Negate]
                       , [ binary "*"  Multiply       Ex.AssocLeft
                         , binary "/"  Divide         Ex.AssocLeft]
                       , [ binary "+"  Plus           Ex.AssocLeft
                         , binary "-"  Minus          Ex.AssocLeft]
                       , [ binary "<"  Less           Ex.AssocNone
                         , binary "<=" LessOrEqual    Ex.AssocNone
                         , binary ">"  Greater        Ex.AssocNone
                         , binary ">=" GreaterOrEqual Ex.AssocNone
                         , binary "==" Equal          Ex.AssocNone
                         , binary "!=" NotEqual       Ex.AssocNone] ]

boolOperatorsTable = [ [ unary "not"  Not]
                     , [ binary "and" And Ex.AssocNone
                       , binary "or"  Or  Ex.AssocNone] ]

operatorsTable = numberOperatorsTable ++ boolOperatorsTable

operatorExpr = Ex.buildExpressionParser operatorsTable operandExpr

-- Everything that possible could be used in operators: everything but not function or assign
-- Helper for `operatorExpr` parser
operandExpr' =  (trySeveral [ variableExprCall
                           , atomicExpr
                           , arrayExpr
                           , (parens operatorExpr) ])
operandExpr = withNext operandExpr'

-- Helper function, that detects call (parentheses and comma-separated values)
withNextCalls :: Parser Expression -> Parser Expression
withNextCalls parser = do
  expr <- parser
  nextArgs <- many (parens $ commaSep expression)
  return $ foldl' (\callable -> \args -> (Call callable args)) expr nextArgs

-- Helper function, that detects subscript (access to array items)
withNextSubscripts :: Parser Expression -> Parser Expression
withNextSubscripts parser = do
  expr <- parser
  nextSubscripts <- many (brackets expression)
  return $ foldl' (\value -> \index -> (Subscript value index)) expr nextSubscripts

withNext parser = do
  originalExpr   <- (lookAhead parser)
  withCalls      <- (lookAhead $ withNextCalls parser)
  withSubscripts <- (lookAhead $ withNextSubscripts parser)

  case (withCalls /= originalExpr, withSubscripts /= originalExpr) of
    (True, _)     -> (withNext $ withNextCalls parser)
    (False, True) -> (withNext $ withNextSubscripts parser)
    _             -> parser

-- Call expressions
variableExprCall = withNextCalls variableExpr


callableExpr' =  trySeveral [ 
                              variableExpr
                            , (parens callableExpr') ]

nonCallableExpr' =  trySeveral [ operatorExpr
                               , atomicExpr
                               , arrayExpr
                               , (parens nonCallableExpr') ]

expression :: Parser Expression
expression = withNext (trySeveral [nonCallableExpr', callableExpr'])


-- ########################
-- ### FLOW EXPRESSIONS ###
-- ########################

ifExpr = do
  let elseParser = (reserved "else" >> blockExpr)

  reserved "if"
  conditional <- parens expression
  trueBranch  <- blockExpr
  falseBranch <- optionMaybe elseParser
  return $ If conditional trueBranch falseBranch

whileExpr = do
  reserved "while"
  conditional <- parens expression
  body        <- blockExpr
  return $ While conditional body



continueExpr = reserved "continue" >> return Continue
breakExpr    = reserved "break"    >> return Break
whileOnlyExpr = trySeveral [breakExpr, continueExpr]

flowExpr = trySeveral [ ifExpr, whileExpr ]


activeObjectExpr = do
    reserved "active"
    name <- identifier
    expression <- braces $ objectDefinitionExpr
    return $ ActiveDef name (Expression expression)

objectDefinitionExpr = do
    let fieldDef = reserved "val" >> identifier
    definitions = 

    


-- ############################
-- ### TOPLEVEL EXPRESSIONS ###
-- ############################

endsWithBlock :: Statement -> Bool
endsWithBlock (While _ _)                   = True
endsWithBlock (If _ _ _)                    = True
endsWithBlock _                             = False

-- General parser to check all expressions available on top-level
simpleStmt = Expression <$> expression
-- statement = trySeveral [flowExpr, simpleStmt]
statement = activeObjectExpr

-- Parser modificator that ensures what expressions should end with semicolon on top-level
ensureSemi :: Parser Statement -> Parser Statement
ensureSemi exprParser = do
  newExpr <- exprParser
  (reservedOp ";" >> return newExpr)

-- Parses many expressions of given parser with correct semicolons
toplevelProducer :: Parser Statement -> Parser [Statement]
toplevelProducer = many . ensureSemi

-- Blocks ( { } ) parsers for given allowed expressions
blockExpr =
  trySeveral [ (braces $ toplevelProducer statement)
             , (blockExprAsSingle $ ensureSemi statement) ]

-- Helper function to allow single expression as block
blockExprAsSingle :: Parser Statement -> Parser [Statement]
blockExprAsSingle exprParser = return <$> exprParser

-- Handle of whitespaces and EOF
contents :: Parser a -> Parser a
contents p = do
  whiteSpace
  r <- p
  eof
  return r

-- Parse given string or return an error
parseToplevel :: String -> Either Parser.ParseError [Statement]
parseToplevel s = parse (contents $ toplevelProducer statement) "<stdin>" s


-- ### ERROR-RELATED FUNCTIONS ###

-- This is done make ParseError visible after Parser.hs import
-- Some kind of encapsulation
-- (only this file really knows what ParseError is)
type ParseError = Error.ParseError