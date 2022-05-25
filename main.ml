(* let lexbuf = Lexing.from_channel stdin 

let ast = Parser.input Lexer.main lexbuf 

let _ = Printf.printf "Parse:\n%s\n" (Ast.as_string ast) *)


open Ast

let parse_file () : automate =
  let file = open_in Sys.argv.(1) in
  let lexbuf = Lexing.from_channel file in
  let parse = Parser.input Lexer.main lexbuf in
  close_in file; parse
;;

let parse = parse_file ();;


type exec = {
    mutable curState: char;  (* l'etat courant *)
    mutable curStack: char list; (* la pile courante *)
    listTrans: (char * char * char * char * (char list)) list (* la liste des transitions *)
  };;
  

let init (a : automate) : exec =
  match a with
    | Automate (Declarations (_, _, _, Initialstate iState, Initialstack iStack), Transitions l) -> 
      {
        curState = iState;
        curStack = [iStack];
        listTrans = l;
      };
;;

let execution = init parse;;



  