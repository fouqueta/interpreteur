type lettre = char

type inputsymbols = Inputsymbols of lettre list

type stacksymbols = Stacksymbols of lettre list

type states = States of lettre list

type initialstate = Initialstate of lettre

type initialstack = Initialstack of lettre

type stack = Stack of lettre list

type transitions = 
  Transitions of (lettre * lettre * lettre * lettre * stack) list

type declarations =
  Declarations of inputsymbols * stacksymbols * states * initialstate * initialstack

type automate = 
  Automate of declarations * transitions

  
let rec as_string = function
  | Inputsymbols l -> "inupts symbols: " ^ parcoursListe l
  | Stacksymbols l -> "stack symbols: " ^ parcoursListe l
  | States l -> "states: " ^ parcoursListe l
  | Initialstate a -> "initial state: " ^ a
  | Initialstack a -> "initial stack symbol:" ^ a
  | Stack l -> "stack" ^ parcoursListe l 
  | Transitions l -> "transitions" ^ parcoursListeTrans l
  | Declarations (i,stck,st,initst,initstck) -> "declarations: " ^ as_string i ^ as_string stck ^ as_string initst ^ as_string initstck
  | Automate (d,t) -> "automate: " ^ as_string d ^ as_string t


and parcoursListe l =
  match l with
  | [] -> ""
  | t::q ->   t ^  string ", " ^ parcoursListe q

and parcoursListeTrans l =
  match l with
  | [] -> ""
  | t::q -> "(" ^  t ^  string "," ^ parcoursListeTrans q ^ ")"

(*)
 and parcoursListeTrans l =
  match l with
  | [] -> ""
  | t::q -> "(" ^ string t ; string "," ; parcoursListeTrans q ^ ")" 
 *)

(* 
let rec as_string = function
  | Inputsymbols ()
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
    else as_string h *)

