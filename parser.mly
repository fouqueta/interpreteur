%{
    open Ast
%}

%token RPAREN LPAREN COMMA SEMICOL INPUTSYMBS STACKSYMBS STATES INITIALSTATE INITIALSTACKSYMB TRANSITIONS
%token<char> LETTRE

%start<Ast.expression> input

%%

  
input: c = automate EOF {}

automate: declarations transitions {}

declarations: inputsymbols stacksymbols states initialstate initialstack {}

inputsymbols: INPUTSYMBS suitelettres-nonvide {}

stacksymbols: STACKSYMBS suitelettres-nonvide {}

states: STATES suitelettres-nonvide {}

initialstate: INITIALSTATE s = LETTRE {} 
 
initialstack: INITIALSTACKSYMB s = LETTRE {}R

suitelettres-nonvide: 
  s = LETTRE {}
| s = LETTRE COMMA suitelettres-nonvide {} 

transitions: TRANSITIONS translist {}

translist: 
  {}
| transition translist {}

transition: LPAREN s1 = LETTRE COMMA lettre-ou-vide COMMA s2 = LETTRE COMMA s3 = LETTRE COMMA stack RPAREN {}

lettre-ou-vide: 
  {} 
| s = LETTRE {}

stack: 
  {}
| nonemptystack {}

nonemptystack: 
  s = LETTRE {}
| s = LETTRE SEMICOL nonemptystack {}