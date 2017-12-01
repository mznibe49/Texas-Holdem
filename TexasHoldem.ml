type couleur =
    Pique
  | Carreau
  | Coeur
  | Trefle
;;

type rang =
    As
  | Roi
  | Dame
  | Valet
  | Dix
  | Neuf
  | Huit
  | Sept
  | Six
  | Cinq
  | Quatre
  | Trois
  | Deux
;;

type carte =
  Carte of rang * couleur
;;

let  c1 = Carte(As,Pique);;
let  c2 = Carte(Roi,Trefle);;


type donnee =
  Donnee of carte * carte
;;

let d = Donnee(c1,c2);;

type table =
    Flop of carte * carte * carte
  | Turn of carte * carte * carte * carte
  | River of carte * carte * carte * carte * carte
;;

type comb =
    Combine of carte * carte * carte * carte * carte
;;


(* 
il faut ajouter les fonction qui renvoie le nom de la combine en
   fonction des cartes 
   ou bien changer le constructeur combine a ce qu'il reçois 
   5 carte et renvoie directement le nom de la combine
   dans l'enoncer ils disent :
   "Les valeurs du type comb doivent encapsuler toutes, et seulement les informations permettant de comparer
efficacement deux combinaisons suivant les règles détaillées en Section 2.1."
*)


(*let check_egal c1 c2 =

  ;;*)

(*let tab = Array.make 13 0;;
*)
(*let compute_comb d t = match t with
  | a as River(c1,c2,c3,c4,c5) -> [a]
  | 
  |
;;
*)


let tab_cards  = [|As;Deux;Trois;
		   Quatre;Cinq; Six;
		   Sept;Huit;
		   Neuf;Dix;
		   Dame;Valet;Roi|]
;;

tab_cards.(0);;


let indice_rang c =
  let res = ref 0 in
  match c with
  | Carte(r1,c1) ->
     for i=0 to (Array.length tab_cards)-1 do
       if (r1 = tab_cards.(i) ) then res := i
     done;
    !res    
;;




(* prend un carte et renvoie une int *)
(*let val_card c =
  let tab = tab_cards in
  match c with
  | Carte(r1,c1) -> tab.(r1)
(*let tab = tab_cards() in*)
 (* match c with
    | Carte(r,c) -> *)
    (* if(r = As ) then  0
       else if ( r = )*)
  (*| Deux -> 1
  | Trois -> 2
  | Quatre  -> 3
  | Cinq -> 4
  | Six -> 5
  | Sept -> 6
  | Huit ->  7
  | Neuf -> 8
  | Dix -> 9
  | Dame -> 10
  | Valet -> 11
    | Roi -> 12*)
;;

  let c = val_card (Carte(As,Pique));;*)

let compute_comb_max d  =
  let tab =  ref (Array.make 13 0) in
  match d with
  | Donnee(c1,c2) -> tab.(indice_rang(c1)) := tab.(indice_rang(c1))+1;
    tab.(indice_rang(c2)) := tab.(indice_rang(c2))+1;
    tab;
      
  
(*for i=0 to 12 do :*)
    
;;


(* user un iventaire *)
let compare_comb c1 c2 =
  
;;


let compare_hands d1 d2 t =
  
;;
  
