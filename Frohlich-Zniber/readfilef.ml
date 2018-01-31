open Compute

(* Conversions necessaires à ce fichier *)

let string_to_rang s =
  if s = "A" then As
  else if s = "R" then Roi
  else if s = "D" then Dame
  else if s = "V" then Valet
  else if s = "10" || s = "9" || s = "8" || s = "7" || s = "6" || s = "5" || s = "4" || s = "3" || s = "2" then Value (int_of_string s)
  else raise Rang_exception
;;

let string_to_couleur s =
  if s = "p" then Pique
  else if s = "ca" then Carreau
  else if s = "co" then Coeur
  else if s = "t" then Trefle
  else raise Couleur_exception
;;

(* ----------------------- *)

exception Format_error

(* Créé une carte à partir de l'information donnée sur le string de la carte s *)
let create_card s =
  if String.get s (String.length(s) - 1) = 'p' || String.get s (String.length(s) - 1) = 't'
  then
    begin
      let s1 = (String.sub s 0 (String.length(s) - 1))
      and s2 = String.make 1 (String.get s (String.length(s) - 1))
      in Carte(string_to_rang s1, string_to_couleur s2)
    end
  else if String.get s (String.length(s) - 2) = 'c'
       && (String.get s (String.length(s) - 1) = 'a' || String.get s (String.length(s) - 1) = 'o')
  then
    begin
      if String.get s 1 = '0'
      then
	begin
	  let s1 = (String.sub s 0 2)
	  and s2 = (String.sub s 2 (String.length (s) - 2) )
	  in Carte(string_to_rang s1, string_to_couleur s2)
	end
      else let s1 = (String.get s 0) and s2 = String.sub s (String.length(s) - 2) (String.length(s) - 1)
	   in Carte(string_to_rang (String.make 1 s1), string_to_couleur s2)
    end
  else raise Format_error
;;

(* Créé une liste de carte à partir d'un string (d'une ligne du fichier par exemple), renvoie Format_error si la ligne comporte des espaces inutiles *)
let create_card_list s =
  let s1 = Str.split (Str.regexp " ") s in
  let rec aux s1 =
    match s1 with
    | [] -> []
    | h :: t -> if h <> "" then create_card h :: aux t else raise Format_error
  in aux s1				
;;

let read_file file =
  let l = ref [] in
  let co = open_in file in
  try
    while true; do
      l := input_line co :: !l
    done; !l
  with End_of_file ->
    close_in co;
    List.rev !l
;;

(* Renvoi true si la liste l comporte un doublon (ou +) *)
let test_duplicate l =
  let rec aux l' =
    match l' with
    | [] -> false
    | h :: t -> if List.mem h t then true else aux t
  in aux l
;;

(* Renvoi true si un élement de l est présent dans l' *)
let test_duplicate_from_list l l' =
  let rec aux l l' =
    match l with
    | [] -> false
    | h :: t -> 
      if List.mem h l' 
      then true 
      else
	aux t l'
  in aux l l'
;;

(* Renvoi une liste composée de liste de carte (par exemple [[donne1];[donne2];[table]]*)
let create_game_from_file file =
  let sl = read_file file in
  let () = if List.length sl <> 3 then raise Format_error in
  let l = ref [] in 
  let rec aux s =
    match s with
    | [] -> []
    | h :: t -> 
      if compare h "?" = 0 
      then [] :: (aux t) 
      else if test_duplicate (create_card_list h) = false
      then
	if test_duplicate_from_list (create_card_list h) !l = false 
	then
	  begin
	    l := !l@(create_card_list h);
	    create_card_list h :: (aux t)
	  end
	else
	  raise Format_error
      else raise Format_error
  in aux sl
;;

let cut_float f =
  let f1 = int_of_float (f *. 1000.) in
  (float_of_int f1) /. 1000.
;;

let string_of_ff ff = match ff with
  | (f1,f2) -> 
    if f1 > f2 
    then "[J1 W] J1: "^(string_of_float (cut_float f1))^" J2: "^(string_of_float (cut_float f2))
    else
      "[J2 W] J1: "^(string_of_float (cut_float f1))^" J2: "^(string_of_float (cut_float f2))
;;

let string_of_ch ch =
  if ch = 1 then "[Joueur 1 Winner]"
  else if ch = -1 then "[Joueur 2 Winner]"
  else "[=] Pas de vainqueur."
;;

(* Mode terminal et/ou toplevel *)
let compute file =
  let llc = create_game_from_file file in
  if List.length (List.nth llc 0) > 0 && List.length (List.nth llc 1) = 0
  then
    "proba_simple \n"
  else if List.length (List.nth llc 2) < 5
  then
    let s = (proba_double (List.nth llc 0) (List.nth llc 1) (List.nth llc 2)) in string_of_ff s
  else
    let s = compare_hands (List.nth llc 0) (List.nth llc 1) (List.nth llc 2) in string_of_ch s
;;

let test_rapide () =
  for i = 1 to 28 do
    let s ="tests_poker/test_"^string_of_int i^".txt"
    in print_string ("Fichier #"^string_of_int i^" "^(compute s))
  done
;;
