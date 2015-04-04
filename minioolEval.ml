type expr = 
    Integer of int
  | Ident of string list (*identity is a list of strings, so x.y.z.w will be represented as [x,y,z,w]*)
  | Null
  | Minus of expr * expr
  | Proc of string * cmd
and
cmd = 
  Dec of string * cmd (*declaration of a variable*) 
  | Call of expr * expr
  | Malloc of string
  | Print of expr (*Not specified in specification, useful for demo, debugging, etc.*)
  | Assign of expr * expr
  | Skip
  | While of bexpr * cmd
  | If of bexpr * cmd * cmd
  | Group of cmd list
  | Atom of cmd
  | Par of cmd list
  | Pop (* helper cmd for popping one frame off stack, not available to MiniOOL programmers *)
and 
bexpr =  (*binary expression *)
    True
  | False
  | Equal of expr * expr
  | LessThan of expr * expr

type program = Program of cmd list

type value = 
    Clo of string * cmd * stack (*lambda expr*)
  | Int of int
  | Field of string
  | ValNull
  | Obj of (string * value ref) list (* objects are represented as list of (string, vref)s *)
and stack = Stack of stackFrame list
and stackFrame = 
  Decl of string * (value ref)
  | CallFrame of string * (value ref) * stack

exception StackNotFound of string

(* returns true if the frame stores ident *)
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

  (*state is defined as stack. the heap is not needed: ocaml runtime manages that for us*)
type state = State of stack

let rec valToStr v = match v with
| Clo (arg, _, _) -> "Clojure: " ^ arg ^ ": <code>"
| ValNull -> "Null"
| Obj l -> String.concat "\n" (List.map (fun (s, v) -> s ^ ": " ^ (valToStr !v)) l)
| Int i -> string_of_int i 
| Field s -> "Field" ^ s
  
let print_value v = print_string (valToStr v ^ "\n")

(*for debugging... print one frame*)
let print_frame f = match f with
  | Decl (s, vref) -> print_string s; print_string " -> "; print_value !vref
  | CallFrame (s, vref, _) -> print_string s; print_string " -> "; print_value !vref

let print_state (State (Stack s)) = List.fold_left (fun _ x -> print_string "\t"; print_frame x) () s 

let stackOfState (State s) = s
let pushIdent i (State (Stack s)) = State (Stack (Decl (i, ref ValNull)::s))
let pushIdentWithVal i v (State (Stack s)) = State (Stack (Decl (i, ref v)::s))
exception PoppingEmptyStack
(*pop the first frame on the stack*)
let popIdent (State (Stack s)) = match s with
 | [] -> raise PoppingEmptyStack
 | h::r -> match h with
   | Decl _ -> State (Stack r)
   | CallFrame (_, _, old) -> (State old) (*reinstall the old stack if we are done with the call frame*)

(*retrieve value on the stack*)
let stateFind i (State s) = stackFind i s

let initialState = State (Stack [])

type config = 
    Executing of program * state
  | Final of state
  | Error of string

  (*for debugging use*)
let print_config c = match c with
  | Executing (_, s) -> print_state s
  | Final s -> print_state s
  | Error s -> print_string "Error: "; print_string s; print_string "\n"


  (* flip: helper function, flip the two arguments of function f, borrowed from Haskell *)
let flip f a b = f b a

exception NonInteger
let minus v1 v2 = match v1 with
  | Int i -> (match v2 with |Int j -> Int (i - j) 
                            |_ -> raise NonInteger)
  | _ -> raise NonInteger

exception FieldNotFound
(* traverseFields: takes a list of strings, returns the corresponding value reference that is on the heap
 * said list of strings are internal representation of expressions like: x.y.z.w, which will be
 * [x,y,z,w] *)
let rec traverseFields l valueRef = match l with
  | [] -> valueRef
  | (h::r) -> match !valueRef with
    | Obj ol -> (try (traverseFields r (List.assoc h ol))
    (* if value not found on an allocated object, this means we should instantiate it (lazy initialization) *)
            with Not_found -> let newL = ((h, ref ValNull)::ol) in valueRef := Obj newL; traverseFields r (List.assoc h newL))
    | _ ->  raise FieldNotFound

exception InvalidLeftValue
(*retrieve a "left" value, useful for assignment*)
let rec evalExprLeft e s = match e with
  | Ident [i] -> stateFind i s
  | Ident (h::r) -> traverseFields r (stateFind h s)
  | _ -> raise InvalidLeftValue

  (*evaluate a "right" value, useful for other situations...*)
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

exception InvalidCallCmd
(* runCmdAtom: execute one command fully, useful for atomic, returns the stack *)
let rec runCmdAtom c s = let sp = match c with
  | Dec (ident, cc) -> popIdent (runCmdAtom cc (pushIdent ident s))
  | Pop -> popIdent s
  | Malloc ident -> let v = stateFind ident s in v := Obj []; s;
  | Assign (lv, rv) -> (evalExprLeft lv s) := (evalExprRight rv s); s
  | While (b, cc) -> if evalBoolean b s then runCmdAtom c (runCmdAtom cc s) else s
  | If (b, c1, c2) -> runCmdAtom (if evalBoolean b s then c1 else c2) s
  | Skip -> s
  | Group l -> List.fold_left (flip runCmdAtom) s l
  | Print e -> print_value (evalExprRight e s); s
  | Atom cc -> runCmdAtom cc s
  | Par l -> List.fold_left (flip runCmdAtom) s l (*Since we are running atomically, we can slack a little bit about parralism *)
  | Call (e1, e2) -> match evalExprRight e1 s with
    | Clo (i, c, Stack frozenStack) -> let _ = runCmdAtom c (State (Stack (CallFrame (i, ref (evalExprRight e2 s), stackOfState s)::frozenStack))) in s
    | _ -> raise InvalidCallCmd
     in sp
 and
  (* stepCmd :: (list of cmds, state) -> (list of cmds, state) 
   * Execute one "step" *)
   stepCmd (l, s) = match l with
   | [] -> (l, s)
   | h::r -> match h with
    | Pop -> (r, popIdent s)
    | Skip -> (r, s)
    | Dec (ident, cc) -> (cc::Pop::r, pushIdent ident s)
    | Malloc ident -> let v = stateFind ident s in v := Obj []; (r, s)
    | Assign (lv, rv) -> (evalExprLeft lv s) := (evalExprRight rv s); (r, s)
    | While (b, cc) -> if evalBoolean b s then (cc::h::r, s) else (r, s)
    | If (b, c1, c2) -> if evalBoolean b s then (c1::r, s) else (c2::r, s)
    | Group [] -> (r, s)
    | Group (x::y) -> (x::Group y :: r, s) (*bring stuff to the front*)
    | Print e -> print_value (evalExprRight e s); (r, s)
    | Atom c -> let newS = runCmdAtom c s in (r,newS)
    | Par l -> parCmd (List.map (fun cc -> ([cc], s)) l); (r,s)
    | Call (e1, e2) -> match evalExprRight e1 s with
      | Clo (i, c, Stack frozen) ->  (c::Pop::r, State (Stack (CallFrame (i, ref (evalExprRight e2 s), stackOfState s) :: frozen)))
      | _ -> raise InvalidCallCmd
  and
  (* parCmd: helper function for paralelism: 
    * use stepCmd to step each command once, then filter out commands that are fully executed
    * *)
    parCmd lsTuples = match lsTuples with
    | [] -> ()
    | _ -> let rr = (List.map (fun (cl, state) -> stepCmd (cl, state)) lsTuples) (* round-robin fashion parrellism *)
    in parCmd (List.filter (fun (cl, _) -> match cl with [] -> false | _ -> true) rr) (*filter out cmds that are already fully executed*)
  and 
  (*runConfig uses stepCmd to execute cmds, 
   * For atomic operations, use runCmdAtom*)
    runConfig c = match c with
  | Executing (Program [], s) -> Final s
  | Final s -> Final s
  | Error s -> Error s
  | Executing (Program l, s) -> let (r, newS) = stepCmd (l, s) in runConfig (Executing (Program r, newS))

let runProg p = runConfig (Executing (p, initialState))

