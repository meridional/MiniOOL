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
%token Print
%token Var
%token LParen RParen
%token LCParen RCParen
%token Par 

%token EOF

%start prog

%type<MinioolEval.program> prog
%type<MinioolEval.cmd list> cmdlist
%type<MinioolEval.cmd> cmd
%type<MinioolEval.cmd> singleCmd
%type<MinioolEval.cmd> complexCmd
%type<MinioolEval.expr> expr
%type<string list> loc
%type<MinioolEval.bexpr> boolExpr


%left Assign
%left Minus
%left Or
%left And
%left Dot
%left Par
%left Semicolon

%%
prog :
  cmdlist EOF {MinioolEval.Program $1}
;

cmdlist :
  | singleCmd Semicolon cmdlist {$1 :: $3}
  | complexCmd cmdlist {$1 :: $2}
  | singleCmd {[$1]}
  | singleCmd Semicolon {[$1]}
  | complexCmd {[$1]}
;

cmd : 
  | singleCmd {$1}
  | complexCmd {$1}
;

complexCmd :
  | LCParen cmdlist RCParen {MinioolEval.Group $2}
  | LCParen parList RCParen {MinioolEval.Par $2}
;

parList :
  | cmd Par parList {$1::$3}
  | cmd Par cmd {$1::[$3]}
;

singleCmd:
  | While boolExpr cmd {MinioolEval.While ($2, $3)}
  | If boolExpr cmd Else cmd {MinioolEval.If ($2,$3,$5)}
  | Atom LParen cmd RParen {MinioolEval.Atom $3} 
  | Skip {MinioolEval.Skip}
  | Var Ident Semicolon cmd {MinioolEval.Dec ($2, $4)}
  | expr LParen expr RParen {MinioolEval.Call ($1, $3)}
  | Malloc LParen Ident RParen {MinioolEval.Malloc $3}
  | Print LParen expr RParen {MinioolEval.Print $3}
  | expr Assign expr {MinioolEval.Assign ($1, $3)}
;

loc :
  | Ident {[$1]}
  | Ident Dot loc {$1::$3}
;


boolExpr :
  | True  {MinioolEval.True}
  | False {MinioolEval.False} 
  | expr Equal expr {MinioolEval.Equal ($1, $3)}
  | expr Less expr {MinioolEval.LessThan ($1, $3)}
;


expr :
  | loc {MinioolEval.Ident $1}
  | Null {MinioolEval.Null}
  | One {MinioolEval.Integer 1}
  | expr Minus expr {MinioolEval.Minus ($1, $3)}
  | Proc Ident Colon cmd {MinioolEval.Proc ($2, $4)}
;
