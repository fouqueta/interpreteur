type inputsymbols = Inputsymbols of char list

type stacksymbols = Stacksymbols of char list

type states = States of char list

type initialstate = Initialstate of char

type initialstack = Initialstack of char

type instr = 
  | Push of char
  | Change of char
  | Pop
  | Reject

type casnext = Casnext of instr

type castop = 
  | Castopinstr of char*instr
  | Castopnext of char*((char*casnext) list)

type casestate = 
  Statenext of (char*casnext) list
| Statetop of castop list

type state = State of char * casestate 

type program = Program of state list
 
type declarations =
  Declarations of inputsymbols * stacksymbols * states * initialstate * initialstack

type automate = 
  Automate of declarations * program
