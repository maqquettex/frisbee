{
module FrisbeeParser where
import Tokens
}


%name astparser
%tokentype { Token }
%monad { Either String } { >>= } { return }
%error { parseError }

%token
    "active"          { TActive _ }
    "passive"         { TPassive _ }
    "new"             { TNew _ }
    "spawn"           { TSpawn _ }
    "import"          { TImport _ }
    "from"            { TFrom _ }
    typeident         { TTypeIdent _ $$ }
    "Void"            { TVoid _ }
    "def"             { TDef _ }
    "return"          { TReturn _ }
    "val"             { TVal _ }
    "String"          { TString _ }
    "Int"             { TInt _ }
    "Bool"            { TBool _ }
    "?"               { TMaybe _ }
    "["               { TLeftBrack _ }
    "]"               { TRightBrack _ }
    "io"              { TIo _ }
    "if"              { TIf _ }
    "else"            { TElse _ }
    "void"            { TVoidValue _ }
    "true"            { TTrue _ }
    "false"           { TFalse _ }
    "this"            { TThis _ }
    "while"           { TWhile _ }
    integer_literal   { TIntLiteral _ $$ }
    string_literal    { TStringLiteral _ $$ }
    ident             { TIdent _ $$ }
    "{"               { TLeftBrace _ }
    "}"               { TRightBrace _ }
    ","               { TComma _ }
    op                { TOp _ $$}
    comop             { TComOp _ $$ }
    "("               { TLeftParen _ }
    ")"               { TRightParen _ }
    ";"               { TSemiColon _ }
    "."               { TPeriod _ }
    "not"             { TNot _ }
    "="               { TEquals _ }
    "<="              { TWaitMessage _ }
    "!"               { TSendMessage _ }

%left op
%nonassoc comop
%%

Program : 
    ImportDeclList ObjectDeclList { Program $1 $2 }


ImportDeclList :
    "from" ident "import" ImportIdentList ";" ImportDeclList { (ImportDecl $2 $4) : $6 }
    |                                                        { [] }

ImportIdentList :
      typeident "," ImportIdentList { $1 : $3 }
    | typeident                   { [ $1 ] }


ObjectDeclList :
     ObjectDecl ObjectDeclList { $1 : $2 }
    |                           { [] }

ObjectDecl : 
      "active"  typeident "{" FieldDeclList MethodDeclList "}"   { ActiveDecl  $2 $4 $5 }
    | "passive" typeident "{" FieldDeclList MethodDeclList "}"   { PassiveDecl $2 $4 $5 }


FieldDeclList :
      Type ident ";" FieldDeclList { (FieldDecl $1 $2) : $4 }
    |                              { [] }

MethodDeclList :
      MethodDecl MethodDeclList  { $1 : $2 }
    |                            { [] }

MethodDecl : 
    "def" Type ident "(" FormalList ")" "{" StatementList "}" { MethodDecl $2 $3 $5 $8 }

FormalList :
      Type ident                { [(FormalArgs $1 $2)] }
    | Type ident "," FormalList { (FormalArgs $1 $2) : $4 }
    |                           { [] }

Type :
    "val"         { TypeAnonymous }
    | CertainType { $1 }

CertainType :
      CertainType "?"     { TypeMaybe $1 }
    | "[" CertainType "]" { TypeArray $2 }
    | "Void"              { TypeVoid }
    | "Int"               { TypeInt }
    | "String"            { TypeString }
    | "Bool"              { TypeBool }
    | typeident           { TypeIdent $1 }
    

Statement :
    "{" StatementList "}"                                { SList $2 }
    | "if" "(" Exp ")" Statement "else" Statement        { SIfElse $3 $5 $7 }
    | "if" "(" Exp ")" Statement                         { SIfElse $3 $5 (SList []) }
    | "while" "(" Exp ")" Statement                      { SWhile $3 $5 }
    | "return" Exp ";"                                   { SReturn $2 }
    | ident "=" Exp ";"                                  { SEqual $1 $3 }
    | Type ident ";"                                     { SVarDecl $1 $2 }
    | Type ident "=" Exp ";"                             { SVarDeclEqual $1 $2 $4 }
    | Exp "." ident   "=" Exp ";"                        { SEqualField $1 $3 $5 }
    | Exp "!" ident "(" ExpList ")" ";"                  { SSendMessage $1 $3 $5}
    | ident "<=" Exp "!" ident "(" ExpList ")" ";"       { SWaitMessage $1 $3 $5 $7 }
    | Type ident "<=" Exp "!" ident "(" ExpList ")" ";"  { SList [(SVarDecl $1 $2), (SWaitMessage $2 $4 $6 $8)] }
    | ident "[" Exp "]" "=" Exp ";"                      { SArrayEqual $1 $3 $6 }
    | Exp   ";"                                          { SExp $1}


StatementList :
      Statement StatementList   { $1 : $2 }
    | Statement                 { [$1] }
    

Exp : 
    Exp op Exp                           { ExpOp $1 $2 $3}
    | Exp comop Exp                      { ExpComOp $1 $2 $3}
    | Exp "[" Exp "]"                    { ExpArrayGet $1 $3}
    | "[" ExpList "]"                    { ExpArrayValue $2 }
    | Exp "." ident "(" ExpList ")"      { ExpFCall $1 $3 $5}
    | Exp "." ident                      { ExpFieldAccess $1 $3}
    | integer_literal                    { ExpInt $1}
    | string_literal                     { ExpString $1}
    | "void"                             { ExpVoid }
    | "true"                             { ExpBool True}
    | "false"                            { ExpBool False}
    | ident                              { ExpIdent $1}
    | "this"                             { ExpThis }
    | "io"                               { ExpIO }
    | "new" typeident "(" ExpList")"     { ExpNewPassive $2 $4}
    | "spawn" typeident "(" ExpList ")"  { ExpSpawnActive $2 $4}
    | "not" Exp                          { ExpNot $2}
    | "(" Exp ")"                        { ExpExp $2} 

ExpList :
        Exp "," ExpList  { $1 : $3 }  
        | Exp            { [$1] }
        | Exp  ","       { [$1] }
        |                { [] }


{

parseError []     = Left "Wrong program structure"
parseError (hd:_) = Left ("Parse error at line " ++ show(alexGetLineNum(tokenPosn hd))) 


data Program = Program [ImportDecl] [ObjectDecl]
    deriving (Show, Eq)


data ImportDecl = ImportDecl String [ImportIdent]
    deriving (Show, Eq)

type ImportIdent = String


data ObjectDecl
    = ActiveDecl  String [FieldDecl] [MethodDecl]
    | PassiveDecl String [FieldDecl] [MethodDecl]
     deriving (Show, Eq)


data FieldDecl = FieldDecl Type String
    deriving (Show, Eq)


data MethodDecl
    = MethodDecl Type String [FormalArgs] [Statement]
    deriving (Show, Eq)


data FormalArgs = 
    FormalArgs Type String 
    deriving (Show, Eq)

data Type =
      TypeAnonymous  -- 
    | TypeMaybe Type  -- type
    | TypeArray Type  -- type
    | TypeInt  --
    | TypeVoid  --
    | TypeBool  --
    | TypeString  --
    | TypeIdent String  -- name
    deriving (Show, Eq)

data Statement
    = SList [Statement]  -- statements
    | SIfElse Exp Statement Statement  -- condition, if_body, else_body
    | SWhile Exp Statement  -- condition, body
    | SReturn Exp  -- expr
    | SEqual String Exp  -- name, expr
    | SVarDeclEqual Type String Exp  -- type, name, expr
    | SVarDecl Type String  -- type, name
    | SEqualField Exp String Exp  -- object, field, expr
    | SArrayEqual String Exp Exp  -- name, index, expr
    | SSendMessage Exp String [Exp]  -- object, method, args
    | SWaitMessage String Exp String [Exp]  -- result_name, object, method, args
    | SExp Exp  -- expr
    deriving (Show, Eq)


data Exp
    = ExpOp Exp String Exp  -- left, operator, right
    | ExpComOp Exp String Exp  -- left, operator, right
    | ExpArrayGet Exp Exp -- array, index
    | ExpArrayValue [Exp] -- elements
    | ExpFCall Exp String [Exp]  -- object, method, args
    | ExpFieldAccess Exp String  -- object, field
    | ExpInt Int  -- value
    | ExpString String  -- value
    | ExpBool Bool -- value
    | ExpVoid -- value
    | ExpIdent String -- name
    | ExpNewPassive String [Exp]  -- typename, args 
    | ExpSpawnActive String [Exp]  -- typename, args
    | ExpExp Exp -- expr
    | ExpThis  -- 
    | ExpIO  -- 
    | ExpNot Exp  -- operand
    deriving (Show, Eq)

}
