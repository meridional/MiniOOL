
open Parsing;;

let lexbuf = Lexing.from_channel stdin in
let prog = try MinioolYACC.prog MinioolLEX.token lexbuf with Parse_error -> print_string "Syntax error\n"; MinioolEval.Program []
in MinioolEval.runProg prog
