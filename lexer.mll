{
    open Parser
}

let layout = [ ' ' '\t' '\n' ]
let ident_char = [0-9a-zA-Z]

rule main = parse
  | layout          { main lexbuf }
  | ')'			    { RPAREN }
  | '('			    { LPAREN }
  | ','			    { COMMA }
  | ';'			    { SEMICOL }
  | 
  | ident_char		{ LETTRE (Lexing.lexeme lexbuf) }
  | eof			  { EOF }
  | _			    { failwith "unexpected character" }

