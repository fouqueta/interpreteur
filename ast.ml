type inputsymbols = Inputsymbols of char list

type stacksymbols = Stacksymbols of char list

type states = States of char list

type initialstate = Initialstate of char

type initialstack = Initialstack of char

type stack = char list

type transitions = 
  Transitions of (char * char * char * char * stack) list

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

