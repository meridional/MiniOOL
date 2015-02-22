
open Parsing;;

let lexbuf = Lexing.from_channel stdin in
try MinioolYACC.prog MinioolLEX.token lexbuf
with Parse_error -> print_string "Syntax error\n";
