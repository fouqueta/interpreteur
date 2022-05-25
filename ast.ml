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



  
let as_string = function
  | Automate (d,t) -> "HELLO"

