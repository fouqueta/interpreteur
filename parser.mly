%{
    open Ast
%}

%token RPAREN LPAREN COMMA SEMICOL INPUTSYMBS STACKSYMBS STATES INITIALSTATE INITIALSTACKSYMB TRANSITIONS EOF
%token<char> LETTRE

%start<Ast.automate> input

%%

  
input: c = automate EOF { (print_string "input\n"; c) }

automate: d=declarations t=transitions { (print_string "automate\n";  (d, t)) }

declarations: i1=inputsymbols s1=stacksymbols s2=states i2=initialstate i3=initialstack { (print_string "declarations\n"; (i1,s1,s2,i2,i3)) }

inputsymbols: INPUTSYMBS s=separated_list(COMMA,LETTRE) { (print_string "inputsymbols\n"; (s)) }

stacksymbols: STACKSYMBS s=separated_list(COMMA,LETTRE) { (print_string "stacksymbols\n"; (s)) }

states: STATES s=separated_list(COMMA,LETTRE) { (print_string "states\n"; (s)) }

initialstate: INITIALSTATE s=LETTRE { (print_string "initialstate\n"; s) } 
 
initialstack: INITIALSTACKSYMB s=LETTRE { (print_string "initialstack\n"; s) }

(*
suitelettres_nonvide: 
  s = LETTRE { (print_string "suitelettres_nonvide1\n"; [s]) }
| s = LETTRE COMMA l=suitelettres_nonvide { (print_string "suitelettres_nonvide2\n"; s::l) } 
*)

transitions: TRANSITIONS t=list (transition) { (print_string "transitions\n"; (t)) }
(*
translist: 
  { (print_string "translist1\n"; []) }
| t1=transition t2=translist { (print_string "translist2\n"; t1::t2) }*)

transition: LPAREN s1=LETTRE COMMA s2=lettre_ou_vide COMMA s3=LETTRE COMMA s4=LETTRE COMMA s5=separated_list(SEMICOL,LETTRE) RPAREN { (print_string "transition\n"; (s1,s2,s3,s4,s5)) }

lettre_ou_vide: 
  { (print_string "lettre_ou_vide1\n"; ' ') } 
| s = LETTRE { (print_string "lettre_ou_vide2\n"; s) }
(*
stack: 
  { (print_string "stack1\n"; []) }
| n=nonemptystack { (print_string "stack2\n"; n) }

nonemptystack: 
  s = LETTRE { (print_string "nonemptystack1\n"; [s]) }
| s = LETTRE SEMICOL n=nonemptystack { (print_string "nonemptystack2\n"; s::n) }*)
