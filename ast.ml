type lettre = char

type inputsymbols = lettre list

type stacksymbols = lettre list

type states = lettre list

type initialstate = lettre

type initialstack = lettre

type stack = lettre list

type transitions =  (lettre * lettre * lettre * lettre * stack) list

type declarations = inputsymbols * stacksymbols * states * initialstate * initialstack

type automate = declarations * transitions



let rec parcoursListe l =
  match l with
  | [] -> ""
  | t::q ->   t ^   ", " ^ parcoursListe q

let rec parcoursListeStack l =
  match l with
  | [] -> ""
  | t::q -> t ^ ";" ^ parcoursListeStack q

  let rec parcoursListeTrans l = 
    match l with
    | [] -> ""
    | t::q -> ";"^t^parcoursListeTrans q


let as_string_inputSymb l =  "input symbols: " ^ parcoursListe l

let as_string_stackSymb l = "stack symbols: " ^ parcoursListe l

let as_string_state l = "states: " ^ parcoursListe l

let as_string_initStack a = "initial stack: " ^ a

let as_string_initState a = "initial state:" ^ a

let as_string_stack l = parcoursListeStack l 

let as_string_declaration (i,stck,st,initst,initstck) = as_string_inputSymb i ^"\n"^ as_string_stackSymb stck ^"\n"^ as_string_state st ^"\n"^ as_string_initState initst ^"\n"^ as_string_initStack initstck^"\n"

let as_string_transition l = 
  match l with
  | [] -> ""
  | t::q -> "transitions: \n"^t^parcoursListeTrans q

let as_string_automate (declaration,transitions) = "automate: " ^ as_string_declaration declaration ^ as_string_transition transitions



















(* and parcoursListeTrans l =
  match l with
  | [] -> ""
  | t::q -> "(" ^ string t ; string "," ; parcoursListeTrans q ^ ")" 
 *)

