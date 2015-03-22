type expr = 
    Integer of int
  | Ident of string list
  | Null
  | Minus of expr * expr
  | Proc of string * cmd
and
cmd = 
    Dec of string * cmd
  | Call of expr * expr
  | Malloc of string
  | Assign of expr * expr
  | Skip
  | While of bexpr * cmd
  | If of bexpr * cmd * cmd
  | Group of cmd list
  | Atom of cmd
  | Par of cmd list
and 
bexpr = 
    True
  | False
  | Equal of expr * expr
  | LessThan of expr * expr
  | GreaterThan of expr * expr

type program = Program of cmd list

type value = 
    Clo of string * cmd * stack
  | Int of int
  | Field of string
  | ValNull
  | Obj of (string * value ref) list
and stack = Stack of stackFrame list
and stackFrame = 
  Decl of string * (value ref)
  | CallFrame of string * (value ref) * stack

exception StackNotFound of string

let frameCompare ident frame = match frame with
  | Decl (s, _) -> ident = s
  | CallFrame (s, _, _) ->  ident = s

let frameValueRef frame = match frame with
  | Decl (_, v) -> v
  | CallFrame (_, v, _) -> v

(* stackFind :: string -> stack -> HeapValue *)
let rec stackFind ident (Stack l) = match l with
  | [] -> raise (StackNotFound ident)
  | (h::r) -> if frameCompare ident h then frameValueRef h else stackFind ident (Stack r)

type state = State of stack

let stackOfState (State s) = s
let pushIdent i (State (Stack s)) = State (Stack (Decl (i, ref ValNull)::s))
let pushIdentWithVal i v (State (Stack s)) = State (Stack (Decl (i, ref v)::s))
let popIdent (State (Stack s)) = State (Stack (List.tl s))
let stateFind i (State s) = stackFind i s

let initialState = State (Stack [])

type config = 
    Executing of program * state
  | Final of state
  | Error of string

let flip f a b = f b a


exception NonInteger

let minus v1 v2 = match v1 with
  | Int i -> (match v2 with |Int j -> Int (i - j) 
                            |_ -> raise NonInteger)
  | _ -> raise NonInteger

exception FieldNotFound

let rec traverseFields l valueRef = match l with
  | [] -> valueRef
  | (h::r) -> try (match !valueRef with
    | Obj l -> traverseFields r (List.assoc h l)
    | _ -> raise FieldNotFound) with _ -> raise FieldNotFound

exception InvalidLeftValue
let rec evalExprLeft e s = match e with
  | Ident [i] -> stateFind i s
  | Ident (h::r) -> traverseFields r (stateFind h s)
  | _ -> raise InvalidLeftValue

let rec evalExprRight e s = match e with
  | Integer i -> Int i
  | Minus (e1, e2) -> minus (evalExprRight e1 s) (evalExprRight e2 s)
  | Ident _ -> !(evalExprLeft e s)
  | Proc (i, c) -> Clo (i, c, stackOfState s)
  | _ -> ValNull

let lessThan v1 v2 = match v1 with
  | Int i -> (match v2 with 
                    | Int j -> i < j
                    | _ -> raise NonInteger)
  | _ -> raise NonInteger
let evalBoolean b s = match b with
  | True -> true
  | False -> false
  | Equal (e1, e2) -> (evalExprRight e1 s) = (evalExprRight e2 s)
  | LessThan (e1, e2) -> lessThan (evalExprRight e1 s) (evalExprRight e2 s)
  | GreaterThan (e2, e1) -> lessThan (evalExprRight e1 s) (evalExprRight e2 s)

exception InvalidCallCmd
let rec runCmd c s = match c with
  | Dec (ident, cc) -> popIdent (runCmd cc (pushIdent ident s))
  | Malloc ident -> let v = stateFind ident s in v := Obj []; s
  | Assign (lv, rv) -> (evalExprLeft lv s) := (evalExprRight rv s); s
  | While (b, cc) -> if evalBoolean b s then runCmd c (runCmd cc s) else s
  | If (b, c1, c2) -> runCmd (if evalBoolean b s then c1 else c2) s
  | Skip -> s
  | Group l -> List.fold_left (flip runCmd) s l
  | Atom cc -> runCmd c s
  | Par l -> List.fold_left (flip runCmd) s l
  | Call (e1, e2) -> match evalExprRight e1 s with
    | Clo (i, c, frozenStack) -> let _ = runConfig (Executing (Program [c], pushIdentWithVal i (evalExprRight e2 s) (State frozenStack))) in s
    | _ -> raise InvalidCallCmd
  and runConfig c = match c with
  | Executing (Program [], s) -> Final s
  | Final s -> Final s
  | Error s -> Error s
  | Executing (Program (h::r), s) -> match h with
    | _ -> let newS = runCmd h s in runConfig (Executing (Program r, newS))

let runProg p = runConfig (Executing (p, initialState))

