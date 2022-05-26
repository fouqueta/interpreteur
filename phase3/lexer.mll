{
    open Parser
}

let layout = [ ' ' '\t' '\n' ]
let ident_char = ['0'-'9' 'a'-'z' 'A'-'Z']

rule main = parse
  | layout      { main lexbuf }
  | ','     { COMMA }
  | ':'     { COLON }
  | "input symbols:"    { INPUTSYMBS }
  | "stack symbols:"    { STACKSYMBS }
  | "states:"    { STATES }
  | "initial state:"    { INITIALSTATE }
  | "initial stack symbol:"     { INITIALSTACKSYMB }
  | "program:"      { PROGRAM } 
  | "case state of"      { CASESTATEOF }
  | "case next of"      { CASENEXTOF }
  | "case top of"      { CASETOPOF }
  | "push"      { PUSH }
  | "change"      { CHANGE }
  | "pop"      { POP }
  | "begin"      { BEGIN }
  | "end"      { END }
  | "reject"      { REJECT }
  | ident_char as s		{ LETTRE (s) }
  | eof     { EOF }
  | _       { failwith "unexpected character" }
