(* type automate =
  | Var of char
  | InputSymb of list * Var
  | StackSymb of list * Var
  | States of list * Var
  | InitState of Var
  | InitStack of Var
  | Transitions of string



let rec transition = function
| Var x -> x
| LPARENT of char??????????????



type expression =
  | Var of string
  | Or of expression * expression
  | And of expression * expression
  | Let of string * expression * expression
  | Let_declaration of string * expression
  | Let_list of expression list * expression

let rec as_string = function
  | Var x -> x
  | True -> "true"
  | False -> "false"
  | Or (l, r) -> apply "\\/" l r
  | And (l, r) -> apply "/\\" l r
  | Let (x, e1, e2) -> apply_let x e1 e2
  | Let_declaration (x, e) -> apply_let_declaration x e
  | Let_list (l, r) -> apply_let_list l r

and apply op l r = 
  "(" ^ as_string l ^ ") " ^ op ^ " (" ^ as_string r ^ ")"

and apply_let variable expr1 expr2 =
  "Let " ^ variable ^ " = " ^ as_string expr1 ^ " in " ^ as_string expr2

and apply_let_declaration variable expr =
  variable ^ " = " ^ as_string expr

and apply_let_list l r =
  "Let " ^ apply_list l ^ " in " ^ as_string r

and apply_list l =
  match l with 
  | [] -> ""
  | h::q ->
    if q <> [] then as_string h ^ " and " ^ apply_list q
    else as_string h
 *)
