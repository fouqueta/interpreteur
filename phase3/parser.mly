%{
    open Ast
%}

%token COMMA COLON INPUTSYMBS STACKSYMBS STATES INITIALSTATE INITIALSTACKSYMB 
%token PROGRAM CASESTATEOF CASENEXTOF CASETOPOF PUSH CHANGE POP BEGIN END REJECT EOF
%token<char> LETTRE

%start<Ast.automate> input

%%

  
input: c = automate EOF { c }

automate: d=declarations p=program { Automate (d, p) }

declarations: i1=inputsymbols s1=stacksymbols s2=states i2=initialstate i3=initialstack 
    { Declarations (i1,s1,s2,i2,i3) }

inputsymbols: INPUTSYMBS s=suitelettres_nonvide { Inputsymbols s }

stacksymbols: STACKSYMBS s=suitelettres_nonvide { Stacksymbols s }

states: STATES s=suitelettres_nonvide { States s }

initialstate: INITIALSTATE s=LETTRE { Initialstate s } 
 
initialstack: INITIALSTACKSYMB s=LETTRE { Initialstack s }

suitelettres_nonvide: 
  s = LETTRE { [s] }
| s = LETTRE COMMA l=suitelettres_nonvide { s::l } 

program: PROGRAM CASESTATEOF s=stateslist { Program s }

stateslist: 
  { [] }
| s1=state s2=stateslist { s1::s2 }

state: s=LETTRE COLON d=distinction_cas { State (s,d) }

distinction_cas:
  d=distinction_cas_next { Statenext d }
| d=distinction_cas_top { Statetop d }

distinction_cas_next: BEGIN CASENEXTOF c=cases_next_of_list END { c }

distinction_cas_top: BEGIN CASETOPOF c=cases_top_of_list END { c }

cases_next_of_list:
  { [] }
| c=case_next_of l=cases_next_of_list { c::l }

case_next_of: s=LETTRE COLON i=instr { (s, Casnext i) }

cases_top_of_list: 
  { [] }
| c=case_top_of l=cases_top_of_list { c::l }

case_top_of: 
  s=LETTRE COLON i=instr { Castopinstr (s,i) }
| s=LETTRE COLON d=distinction_cas_next { Castopnext (s,d) }

instr:
  PUSH s=LETTRE { Push s }
| CHANGE s=LETTRE { Change s }
| POP { Pop }
| REJECT { Reject }
