%{
  (*header*)
%}


%token<string> Ident
%token Dot

%token Minus
%token One

%token Proc
%token Colon

%token True False
%token And Or
%token Equal Less LessEq Greater GreaterEq


%token Null

%token Semicolon
%token Assign
%token If Else
%token While
%token Skip
%token Atom
%token Malloc
%token Var
%token LParen RParen
%token LCParen RCParen
%token Par 

%token EOF

%start prog

%type<unit> prog cmdlist cmd expr loc boolExpr singleCmd complexCmd

%left Assign
%left Minus
%left Or
%left And
%left Dot
%left Par
%left Semicolon

%%
prog :
  cmdlist EOF {}
;

cmdlist :
  | singleCmd Semicolon cmdlist {}
  | complexCmd cmdlist {}
  | singleCmd Semicolon {}
  | singleCmd {}
  | complexCmd {}
;

cmd : 
  | singleCmd {}
  | complexCmd {}
;

complexCmd :
  | LCParen cmdlist RCParen {}
  | LCParen parList RCParen {}
;

parList :
  | cmd Par parList {}
  | cmd Par cmd {}
;

singleCmd:
  | Semicolon {}
  | While boolExpr cmd {}
  | If boolExpr cmd Else cmd {}
  | Atom LParen cmd RParen {} 
  | Skip {}
  | Var Ident {}
  | loc LParen expr RParen {}
  | Malloc LParen Ident RParen {}
  | loc Assign expr {}
;

loc :
  | Ident {}
  | Ident Dot loc {}
;


boolExpr :
  | True  {}
  | False {} 
  | expr rel expr {}
  | boolExpr And boolExpr {}
  | boolExpr Or boolExpr {}
;

rel :
  | Equal {}
  | Less {}
  | LessEq {}
  | Greater {}
  | GreaterEq {}
;

expr :
  | loc {}
  | Null {}
  | One {}
  | expr Minus expr {} 
  | Proc Ident Colon cmd {}
;
