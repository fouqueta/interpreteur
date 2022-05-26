%{
    open Ast
%}

%token COMMA COLON INPUTSYMBS STACKSYMBS STATES INITIALSTATE INITIALSTACKSYMB 
%token PROGRAM CASESTATEOF CASENEXTOF CASETOPOF PUSH CHANGE POP BEGIN END REJECT EOF
%token<char> LETTRE

%start<unit> input

%%

  
input: c = automate EOF { (*((*print_string "input\n"; *)c)*) }

automate: d=declarations p=program { (*((*print_string "automate\n"; *)Automate (d, p))*) }

declarations: i1=inputsymbols s1=stacksymbols s2=states i2=initialstate i3=initialstack 
    { (*((*print_string "declarations\n"; *)Declarations (i1,s1,s2,i2,i3))*) }

inputsymbols: INPUTSYMBS s=suitelettres_nonvide { (*((*print_string "inputsymbols\n"; *)Inputsymbols s)*) }

stacksymbols: STACKSYMBS s=suitelettres_nonvide { (*((*print_string "stacksymbols\n"; *)Stacksymbols s)*) }

states: STATES s=suitelettres_nonvide { (*((*print_string "states\n"; *)States s)*) }

initialstate: INITIALSTATE s=LETTRE { (*((*print_string "initialstate\n"; *)Initialstate s)*) } 
 
initialstack: INITIALSTACKSYMB s=LETTRE { (*((*print_string "initialstack\n"; *)Initialstack s)*) }

suitelettres_nonvide: 
  s = LETTRE { (*((*print_string "suitelettres_nonvide1\n"; *)[s])*) }
| s = LETTRE COMMA l=suitelettres_nonvide { (*((*print_string "suitelettres_nonvide2\n"; *)s::l)*) } 

program: PROGRAM CASESTATEOF s=stateslist {  }

stateslist: 
  {  }
| s1=state s2=stateslist {}

state: s1=LETTRE COMMA distinction_cas { }

distinction_cas:
  distinction_cas_next {}
| distinction_cas_top {}

distinction_cas_next: BEGIN CASENEXTOF cases_next_of_list END {}

distinction_cas_top: BEGIN CASETOPOF cases_top_of_list END {}

cases_next_of_list:
  {}
| case_next_of cases_next_of_list {}

case_next_of: s1=LETTRE COMMA instr {}

cases_top_of_list: 
  {}
| s1=LETTRE COMMA case_top_of cases_top_of_list {}

case_top_of: 
  instr {}
| distinction_cas_next {}

instr:
  PUSH s1=LETTRE {}
| CHANGE LETTRE {}
| POP {}
| REJECT {}
