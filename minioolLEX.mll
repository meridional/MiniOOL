{
  open MinioolYACC;;
}

rule token = parse
  | [' ' '\t' '\n'] { token lexbuf }
  | eof { EOF }
  | ';' { Semicolon }
  | ':' { Colon }
  | "if" { If }
  | "else" { Else }
  | "while" { While }
  | "Atom" { Atom }
  | "|||" { Par }
  | "skip" { Skip }
  | "malloc" { Malloc }
  | "print" { Print }
  | "null" { Null }
  | "var" { Var }
  | "proc" { Proc }
  | "true" { True }
  | "false" { False }
  | "&&" { And }
  | "||" { Or }
  | "==" { Equal }
  | "<" { Less }
  | "<=" { LessEq }
  | ">" { Greater }
  | ">=" { GreaterEq }
  | '-' { Minus }
  | '1' { One }
  | '.' { Dot }
  | '(' { LParen }
  | ')' { RParen }
  | '{' { LCParen }
  | '}' { RCParen }
  | '=' { Assign }
  | (['A'-'Z'] | ['a'-'z'])(['A'-'Z'] | ['a'-'z'] | ['0'-'9'])* as idt { Ident idt }

