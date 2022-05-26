open Ast
open Parser
open Printf

let parse_file () : automate =
  let file = open_in Sys.argv.(1) in
  let lexbuf = Lexing.from_channel file in
  let parse = Parser.input Lexer.main lexbuf in
  close_in file; parse
;;

let parse = try parse_file () with Parser.Error -> fprintf stderr "Syntax error\n"; exit (-1);;


type exec = {
    mutable curState: char;  (* l'etat courant *)
    mutable curStack: char list; (* la pile courante *)
    mutable listTrans: (char * char * char * char * (char list)) list (* la liste des transitions *)
  };;
  

let init (a : automate) : exec =
  match a with
    | Automate (Declarations (_, _, _, Initialstate iState, Initialstack iStack), Transitions l) -> 
      {
        curState = iState;
        curStack = [iStack]; (* on decide haut de la pile a gauche *)
        listTrans = l;
      };
;;


(* affichage du detail de l'execution *)
let affichageExecution (e1 : char) (c : char) (e2 : char) (env : exec) =
  printf "Passage de l'etat %c a l'etat %c\n" e1 e2;
  if c = ' ' then printf "Epsilon transition\n"
  else printf "La lettre '%c' a ete consommee\n" c;
  printf "Etat de la pile apres transition :";
  if env.curStack = [] then printf " pile vide";
  let rec affichageStack s = 
    match s with
    |[] -> printf "\n\n"
    |h::t -> printf " %c" h; affichageStack t
  in affichageStack env.curStack 
;;

(* fait les maj de l'environnement apres application d'une transition (etat courant et pile) *)
let applyTransition (e : char) (s : char list) (env : exec) =
  env.curState <- e;
  match s with 
  |[] -> begin match env.curStack with
          |[] -> env.curStack <- []
          |h'::t' -> env.curStack <- t' end
  |h::t -> env.curStack <- List.rev_append t env.curStack 
;;

(* teste si une transition est applicable, si oui met a jour l'environnement *)
let check1Transition (e1 : char) (c : char) (s1 : char) (e2 : char) (s2 : char list) (lettre : char) (env : exec) : bool =
  if (env.curState = e1 && (List.hd env.curStack = s1) && (c = ' ' || c = lettre)) then
    begin applyTransition e2 s2 env; affichageExecution e1 c e2 env; true end
  else false
;;

(* teste si une lettre peut etre consommee en parcourant toutes les transitions *)
(* on sort de la boucle recursive que si erreur ou si transition pour la lettre est trouvee, on continue meme si epsilon transition est trouvee *)
let rec checkAllTransitions (lettre : char) listTr (env : exec) : unit =
  match listTr with
  |[] -> fprintf stderr "Erreur : pas de transition possible\nMot rejeté\n"; exit (-1)
  |(e1,c,s1,e2,s2)::t ->
    begin if (c = ' ' && check1Transition e1 c s1 e2 s2 lettre env) then checkAllTransitions lettre t env
    else if (c = lettre && check1Transition e1 c s1 e2 s2 lettre env) then ()
    else checkAllTransitions lettre t env end
;;

(* apres que le soit soit fini, on essaie de vider la pile *)
let rec checkTransToEmptyStack listTr (env : exec) : unit =
  match listTr with
  |[] -> fprintf stderr "Erreur : pas de transition possible\nMot rejeté\n"; exit (-1)
  |(e1,c,s1,e2,s2)::t ->
    begin if (c = ' ' && check1Transition e1 c s1 e2 s2 ' ' env) then ()
    else checkTransToEmptyStack t env end
;;


(*  execute l'automate sur le mot, dit s'il est accepte ou non par la grammaire *)
let etapeExec (mot : string) (env : exec) =
  for i = 0 to String.length mot - 1 do
    let lettre = String.get mot i in
    match env.listTrans with
    |[] -> fprintf stderr "Erreur : liste de transitions vide\n"; exit (-1)
    |h::_ -> checkAllTransitions lettre env.listTrans env;
    if (env.curStack = [] && i <> String.length mot -1) then begin
      fprintf stderr "Erreur : pile vide avant la fin du mot\nMot rejeté\n"; exit (-1); end
  done;
  printf "Fin du mot\n\n";
  while List.length env.curStack <> 0 do
    checkTransToEmptyStack env.listTrans env
  done;
  printf "Mot %s accepté !\n\n" mot
;;  


try
  while true do
    let env = init parse in 
    print_string "Rentrez un mot : ";
    let chaine = read_line () in
    if chaine = "exit" then raise Exit
    else etapeExec chaine env;
  done
with Exit -> exit 0;;







  