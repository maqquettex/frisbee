module Lexer where

import Text.Parsec.Char
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)

import qualified Text.Parsec.Token as Tok

lexer :: Tok.TokenParser ()

lexer = Tok.makeTokenParser languageDef
  where
    operators = [ "+", "*", "-", "/"                -- math operators
                , "<", "<=", ">", ">=", "==", "!="  -- compare operators
                , "and", "or", "not"                -- boolean operators
                , "=" ]                             -- assign
    names = [ "true", "false", "null"        -- predefined values
            , "if", "else"                   -- if-expression
            , "function", "return"           -- function-related statemenrs
            , "while", "break", "continue"   -- loop-related statements
            , "print", "isnull", "length" ]  -- builtin functions
    languageDef = emptyDef {
                Tok.commentLine = "//"
              , Tok.commentStart = "/*"
              , Tok.commentEnd = "*/"
              , Tok.reservedOpNames = operators
              , Tok.reservedNames = names
              , Tok.identStart = letter
              , Tok.identLetter = alphaNum  -- snake case now supported
              }

-- Values
integer = Tok.integer lexer
float = Tok.float lexer
stringLiteral = Tok.stringLiteral lexer

-- Variables
identifier = Tok.identifier lexer

-- Grouping tokens
parens = Tok.parens lexer
braces = Tok.braces lexer
brackets = Tok.brackets lexer

-- Separators
semiSep = Tok.semiSep lexer
commaSep = Tok.commaSep lexer
semi = Tok.semi lexer
dot = Tok.dot lexer
whiteSpace = Tok.whiteSpace lexer

-- Reserved names and operators
reserved = Tok.reserved lexer
reservedOp = Tok.reservedOp lexer
