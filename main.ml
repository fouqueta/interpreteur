open Ast
open Parser
open Printf

let parse_file () : automate =
  let file = open_in Sys.argv.(1) in
  let lexbuf = Lexing.from_channel file in
  let ast = Parser.input Lexer.main lexbuf in
  close_in file; ast
;;

let ast = try parse_file () with Parser.Error -> eprintf "Syntax error\n"; exit (1);;

type exec = {
    mutable curState: char;  (* l'etat courant *)
    mutable curStack: char list; (* la pile courante *)
    listTrans: (char * char * char * char * (char list)) list; (* la liste des transitions *)
    listStack: char list; (* la liste des symboles de piles *)
    listStates: char list; (* la liste des etats *)
  };;
  

let init (a : automate) : exec =
  match a with
    | Automate (Declarations (_, Stacksymbols lStack, States lstates, Initialstate iState, Initialstack iStack), Transitions l) -> 
      {
        curState = iState;
        curStack = [iStack]; (* on decide haut de la pile a gauche *)
        listTrans = l;
        listStack = lStack;
        listStates = lstates;
      };
;;


(* etat initial doit etre un element de l'ensemble des etats *)
let rec appartientEtats (l : char list) (env : exec) =
  match l with
  | [] -> false
  | h::t -> h = env.curState || appartientEtats t env
;;

(* le symbole de pile initial doit etre dans l’ensemble des symboles de pile *)
let rec appartientPile (l : char list) (env : exec) =
  match l with
  | [] -> false
  | h::t -> h = List.hd env.curStack || appartientPile t env
;;


(* l’automate doit etre deterministe *)
let rec estDeterministe l = 
  match l with
  | [] -> true
  | (e1,c,s1,_,_)::t -> let rec auxDeterministe l' (etat : char) (lettre : char) (pile : char) =
            begin match l' with
            | [] -> true
            | (e2,c2,s2,_,_)::t1 -> begin
                if (etat=e2 && pile=s2 && (lettre=c2 || lettre=' ' || c2=' ')) then false
                else auxDeterministe t1 e2 c2 s2 end
            end in auxDeterministe t e1 c s1 && estDeterministe t
;;

(* affichage du detail de l'execution *)
let affichageExecution (e1 : char) (c : char) (e2 : char) (env : exec) =
  printf "Passage de l'état %c à l'état %c\n" e1 e2;
  if c = ' ' then printf "Epsilon transition\n"
  else printf "La lettre '%c' a été consommée\n" c;
  printf "Etat de la pile après transition :";
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
  |[] -> eprintf "Erreur : pas de transition possible\nMot rejeté\n"; exit (1)
  |(e1,c,s1,e2,s2)::t ->
    begin if (c = ' ' && check1Transition e1 c s1 e2 s2 lettre env) then checkAllTransitions lettre t env
    else if (c = lettre && check1Transition e1 c s1 e2 s2 lettre env) then ()
    else checkAllTransitions lettre t env end
;;

(* apres que le soit soit fini, on essaie de vider la pile *)
let rec checkTransToEmptyStack listTr (env : exec) : unit =
  match listTr with
  |[] -> eprintf "Erreur : pas de transition possible\nMot rejeté\n"; exit (1)
  |(e1,c,s1,e2,s2)::t ->
    begin if (c = ' ' && check1Transition e1 c s1 e2 s2 ' ' env) then ()
    else checkTransToEmptyStack t env end
;;


(*  execute l'automate sur le mot, dit s'il est accepte ou non par la grammaire *)
let etapeExec (mot : string) (env : exec) =
  for i = 0 to String.length mot - 1 do
    let lettre = String.get mot i in
    match env.listTrans with
    |[] -> eprintf "Erreur : liste de transitions vide\n"; exit (1)
    |h::_ -> checkAllTransitions lettre env.listTrans env;
    if (env.curStack = [] && i <> String.length mot -1) then begin
      eprintf "Erreur : pile vide avant la fin du mot\nMot rejeté\n"; exit (1); end
  done;
  printf "Fin du mot\n\n";
  while List.length env.curStack <> 0 do
    checkTransToEmptyStack env.listTrans env
  done;
  printf "Mot %s accepté !\n\n" mot
;;  


let boucle () =
  try
    while true do
      let env = init ast in 
      print_string "Rentrez un mot : ";
      let chaine = read_line () in
      if chaine = "exit" then raise Exit
      else etapeExec chaine env;
    done
  with Exit -> exit 0;;
;;

let env = init ast in 
if not(appartientEtats env.listStates env) then begin
  eprintf "Erreur : l’état initial n'est pas un élément de l’ensemble des états\n"; exit (1) end
else if not(appartientPile env.listStack env) then begin
  eprintf "Erreur : le symbole de pile initial n'est pas dans l’ensemble des symboles de pile\n"; exit (1) end
else if not(estDeterministe env.listTrans) then begin
  eprintf "Erreur : l'automate n'est pas déterministe\n"; exit (1) end
else  boucle ();;







  