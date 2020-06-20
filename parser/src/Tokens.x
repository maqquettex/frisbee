
{
{-# LANGUAGE ViewPatterns #-}
module Tokens where
}

%wrapper "posn"

$digit   = 0-9
$alpha   = [a-zA-Z]
$lwalpha = [a-z]
$upalpha = [A-Z]
$graphic = [$printable $white]

@string  = \" ($graphic # \")* \"
@comment = \# ($graphic # \")* \#



tokens :-

  $white+                       ;  -- Ignore whitespaces
  "active"                      { \p s -> TActive p }
  "passive"                     { \p s -> TPassive p }
  "new"                         { \p s -> TNew p }
  "spawn"                       { \p s -> TSpawn p }
  "import"                      { \p s -> TImport p }
  "from"                        { \p s -> TFrom p }
  
  "return"                      { \p s -> TReturn p }
  "def"                         { \p s -> TDef p }
  
  "Void"                        { \p s -> TVoid p }
  "val"                         { \p s -> TVal p }
  "String"                      { \p s -> TString p }
  "Int"                         { \p s -> TInt p }
  "Bool"                        { \p s -> TBool p }
  $upalpha[$alpha $digit]*      { \p s -> TTypeIdent p s }
  "?"                           { \p s -> TMaybe p }
  
  "io"                          { \p s -> TIo p }
  "if"                          { \p s -> TIf p }
  "else"                        { \p s -> TElse p }
  "void"                        { \p s -> TVoidValue p }
  "true"                        { \p s -> TTrue p }
  "false"                       { \p s -> TFalse p }
  "this"                        { \p s -> TThis p }
  "while"                       { \p s -> TWhile p }
  $digit+                       { \p s -> TIntLiteral p (read s) }
  "."                           { \p s -> TPeriod p }

  "and"                         { \p s -> TOp p "and" }
  "not"                         { \p s -> TNot p }
  "or"                          { \p s -> TOp p "or" }
  
  [\+\-\*\/]                    { \p s -> TOp p [head s] }

  "<"                           { \p s -> TComOp p "<" }
  ">"                           { \p s -> TComOp p ">" }
  "=="                          { \p s -> TComOp p "==" }
  "!="                          { \p s -> TComOp p "!=" }

  "="                           { \p s -> TEquals p }
  "<="                          { \p s -> TWaitMessage p }  -- rewrite to use <<
  "!"                           { \p s -> TSendMessage p }

  ";"                           { \p s -> TSemiColon p }
  "("                           { \p s -> TLeftParen p }
  ")"                           { \p s -> TRightParen p }
  $lwalpha[$alpha $digit \_ ]*  { \p s -> TIdent p s }
  @string                       { \p s -> TStringLiteral p (init (tail s)) -- remove the leading and trailing double quotes }
  @comment                      { \p s -> TComment p s }
  "{"                           { \p s -> TLeftBrace p }
  "}"                           { \p s -> TRightBrace p }
  ","                           { \p s -> TComma p }
  "["                           { \p s -> TLeftBrack p }
  "]"                           { \p s -> TRightBrack p }
{
-- Each action has type ::AlexPosn -> String -> Token

-- The token type:
data Token =
      TLeftBrace {tokenPos :: AlexPosn }
    | TRightBrace {tokenPos :: AlexPosn }
    | TComma {tokenPos :: AlexPosn }
    | TLeftBrack {tokenPos :: AlexPosn }
    | TRightBrack {tokenPos :: AlexPosn }
    | TActive {tokenPos :: AlexPosn }
    | TPassive {tokenPos :: AlexPosn }
    | TDef {tokenPos :: AlexPosn }
    | TString {tokenPos :: AlexPosn }
    | TComment {tokenPos :: AlexPosn, comment :: String}
    | TVal {tokenPos :: AlexPosn }
    | TVoid {tokenPos :: AlexPosn }
    | TVoidValue {tokenPos :: AlexPosn }
    | TInt {tokenPos :: AlexPosn }
    | TBool {tokenPos :: AlexPosn }
    | TIo {tokenPos :: AlexPosn }
    | TIf {tokenPos :: AlexPosn }
    | TElse {tokenPos :: AlexPosn }
    | TTrue {tokenPos :: AlexPosn }
    | TFalse {tokenPos :: AlexPosn }
    | TThis {tokenPos :: AlexPosn }
    | TWhile {tokenPos :: AlexPosn }
    | TNew {tokenPos :: AlexPosn }
    | TSpawn {tokenPos :: AlexPosn }
    | TImport {tokenPos :: AlexPosn }
    | TFrom {tokenPos :: AlexPosn }
    | TOp {tokenPos :: AlexPosn, op :: String }
    | TComOp {tokenPos :: AlexPosn, comop :: String }
    | TMaybe {tokenPos :: AlexPosn }
    | TNot {tokenPos :: AlexPosn }
    | TEquals {tokenPos :: AlexPosn }
    | TWaitMessage {tokenPos :: AlexPosn }
    | TSendMessage {tokenPos :: AlexPosn }
    | TPeriod {tokenPos :: AlexPosn }
    | TSemiColon {tokenPos :: AlexPosn }
    | TLeftParen {tokenPos :: AlexPosn }
    | TRightParen {tokenPos :: AlexPosn }
    | TIdent {tokenPos :: AlexPosn, ident :: String }
    | TTypeIdent {tokenPos :: AlexPosn, typeident :: String }
    | TIntLiteral {tokenPos :: AlexPosn, ival :: Int } 
    | TStringLiteral {tokenPos :: AlexPosn, sval :: String } 
    | TReturn {tokenPos :: AlexPosn }
    deriving (Eq,Show)



               


alexGetLineNum :: Token -> Int
alexGetLineNum (tokenPos -> AlexPn offset lineNum colNum) = lineNum



alexScanTokensCustom :: String -> Either String [Token]
alexScanTokensCustom str = go (alexStartPos,'\n',[], str)
  where go inp@(pos, x, [], str) =
          case alexScan inp 0 of
                AlexEOF -> Right []
                AlexError ((AlexPn _ line _),_,_,_) -> Left ("Line " ++ show line ++ ": lexical error")
                AlexSkip  inp' len     -> go inp'
                AlexToken inp' len act ->
                    (go inp') >>= (\r -> Right $ (act pos (take len str)) : r)

}