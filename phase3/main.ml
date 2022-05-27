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
    listTrans: state list; (* la liste des transitions *)
    (* -> (etatCourant * lettre * topPile * nouvelEtat * nouveauTopPile) list *)
    listStack: char list; (* la liste des symboles de piles *)
    listStates: char list; (* la liste des etats *)
  };;



let init (a : automate) : exec =
  match a with
    | Automate (Declarations (_, Stacksymbols lStack, States lstates, Initialstate iState, Initialstack iStack), Program p) -> 
      {
        curState = iState;
        curStack = [iStack]; (* on decide haut de la pile a gauche *)
        listTrans = p;
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

let doPop (env : exec) =
  match env.curStack with
  |[] -> eprintf "Erreur : impossible de supprimer l'élément au sommet de la pile, elle est vide\n"; exit (1)
  |h::t -> env.curStack <- t
;;


let affichageExecution (c : char) (curEtat : char) (nextEtat : char) (env : exec) =
  printf "Passage de l'état %c à l'état %c\n" curEtat nextEtat;
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

let checkInstr (etat : char) (lettre : char) (i : instr) (env : exec) =
  match i with
  | Push x -> env.curStack <- [x] @ env.curStack; affichageExecution lettre etat etat env
  | Change x -> env.curState <- x; affichageExecution lettre etat x env
  | Pop -> doPop env; affichageExecution lettre etat etat env
  | Reject -> eprintf "Reject rencontré : Mot rejeté\n"; exit (1)
;;

let rec checkStatenext (l : char) (etat : char) (li : (char*casnext) list) (env : exec) : bool =
  match li with
  |[] -> false
  |(lettre, Casnext instruct)::t -> begin
      if (l=lettre) then (checkInstr etat l instruct env; true)
      else checkStatenext l etat t env end
;;

let rec checkStatetop (l : char) (etat : char) (li : castop list) (env : exec) : bool =
  match li with 
  |[] -> false
  |(Castopinstr (hautPile,instruct))::t -> begin
          if (List.hd env.curStack <> hautPile) then checkStatetop l etat t env
          else begin if (instruct = Pop) then (doPop env; affichageExecution l etat etat env;false)
                     else (checkInstr etat l instruct env; true)
               end 
          end
  |(Castopnext (hautPile,actionNext))::t -> begin
          if (List.hd env.curStack <> hautPile) then checkStatetop l etat t env
          else checkStatenext l etat actionNext env
          end
  
;;

let rec checkAllTransitions (l : char) (s : state list) (env : exec) =
  match s with
  |[] -> eprintf "Erreur : pas de transition possible\nMot rejeté\n"; exit (1)
  |(State (etat, Statenext l'))::t -> if (etat <> env.curState || not(checkStatenext l etat l' env)) then 
                                        checkAllTransitions l t env
  |(State (etat, Statetop l'))::t -> if (etat <> env.curState || not(checkStatetop l etat l' env)) then 
                                        checkAllTransitions l t env
;;

let rec checkPop (l : castop list) (etat : char) (env : exec) : bool =
  match l with
  |[] -> false
  |(Castopinstr (hautPile,instruct))::t -> begin
          if (List.hd env.curStack <> hautPile) then checkPop t etat env
          else begin if (instruct = Pop) then (doPop env; affichageExecution ' ' etat etat env;true)
                     else checkPop t etat env
               end 
          end
  |(Castopnext (hautPile,actionNext))::t -> checkPop t etat env
;;

let rec checkTransToEmptyStack (listr : state list) (env : exec) =
  match listr with
  |[] -> eprintf "Erreur : pas de transition possible\nMot rejeté\n"; exit (1)
  |(State (etat, Statenext l))::t -> checkTransToEmptyStack t env
  |(State (etat, Statetop l))::t -> if (etat <> env.curState || not(checkPop l etat env)) then 
                                        checkTransToEmptyStack t env
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
    (* checkAllTransitions ' ' env.listTrans env *)
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
else  boucle ();;
      