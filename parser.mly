%{
    open Ast
%}

%token RPAREN LPAREN COMMA SEMICOL INPUTSYMBS STACKSYMBS STATES INITIALSTATE INITIALSTACKSYMB TRANSITIONS EOF
%token<char> LETTRE

%start<unit> input

%%

  
input: c = automate EOF { print_string "input" }

automate: declarations transitions {}

declarations: inputsymbols stacksymbols states initialstate initialstack {}

inputsymbols: INPUTSYMBS suitelettres_nonvide {}

stacksymbols: STACKSYMBS suitelettres_nonvide {}

states: STATES suitelettres_nonvide {}

initialstate: INITIALSTATE s = LETTRE {} 
 
initialstack: INITIALSTACKSYMB s = LETTRE {}

suitelettres_nonvide: 
  s = LETTRE {}
| s = LETTRE COMMA suitelettres_nonvide {} 

transitions: TRANSITIONS translist {}

translist: 
  {}
| transition translist {}

transition: LPAREN s1 = LETTRE COMMA lettre_ou_vide COMMA s2 = LETTRE COMMA s3 = LETTRE COMMA stack RPAREN {}

lettre_ou_vide: 
  {} 
| s = LETTRE {}

stack: 
  {}
| nonemptystack {}

nonemptystack: 
  s = LETTRE {}
| s = LETTRE SEMICOL nonemptystack {}