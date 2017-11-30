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

(* il faut ajouter les fonction qui renvoie le nom de la combine en
   fonction des cartes 
   ou bien changer le constructeur combine a ce qu'il reçois 
   5 carte et renvoie directement le nom de la combine
   dans l'enoncer ils disent :
   "Les valeurs du type comb doivent encapsuler toutes, et seulement les informations permettant de comparer
efficacement deux combinaisons suivant les règles détaillées en Section 2.1."

*)
