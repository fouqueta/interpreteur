{
    open Parser
}

let layout = [ ' ' '\t' '\n' ]
let ident_char = ['0'-'9' 'a'-'z' 'A'-'Z']

rule main = parse
  | layout      { main lexbuf }
  | ')'     { RPAREN }
  | '('     { LPAREN }
  | ','     { COMMA }
  | ';'     { SEMICOL }
  | "input symbols:"    { INPUTSYMBS }
  | "stack symbols:"    { STACKSYMBS }
  | "states:"    { STATES }
  | "initial state:"    { INITIALSTATE }
  | "initial stack symbol:"    { INITIALSTACKSYMB }
  | "transitions:"      { TRANSITIONS } 
  | ident_char as s		{ LETTRE (s) }
  | eof     { EOF }
  | _       { failwith "unexpected character" }
