type couleur = Pique | Carreau | Coeur | Trefle | Vide
type rang = Value of int | Valet | Dame | Roi | As | Vide
type carte = Carte of rang * couleur
type donne = carte list
type table = carte list
type comb =
    Carte_haut of rang list
  | Paire of rang * rang * rang * rang
  | Double_Paire of rang * rang * rang
  | Brelan of rang * rang * rang
  | Suite of rang
  | Couleur of rang list
  | Full of rang * rang
  | Carre of rang * rang
  | Quinte_flush of rang
exception Rang_exception
exception Couleur_exception
val val_rang : rang -> int
val int_to_rang : int -> rang
val int_to_couleur : int -> couleur
val couleur_to_int : couleur -> int
val string_of_col : int -> string
val string_of_rang : int -> string
val string_of_carte : carte -> string
val modifier_tab : int array -> rang -> int array
val occ : carte list -> carte list -> int array
val is_Carre : int array -> bool
val is_Brelan : int array -> bool
val is_Full : int array -> bool
val is_Paire : int array -> bool
val is_Double_Paire : int array -> bool
val as_one : int array -> bool
val indice_last : int array -> int
val is_Suite : int array -> bool
val is_Col : carte list -> couleur -> int
val is_Couleur : carte list -> bool
val res_couleur : carte list -> couleur
val is_Quinte_flush : int array -> carte list -> carte list -> bool
val valeur_best_comb : int array -> carte list -> carte list -> int
val indice_carte : 'a array -> 'a -> int -> int
val carre_b : int array -> int -> int
val exist_3 : int array -> int -> bool
val full_b : int array -> int -> int
val dp_b : int array -> int -> int -> int
val make_qf : carte list -> carte list -> comb
val make_carre : int array -> comb
val make_full : int array -> comb
val t : comb
val make_suite : int array -> comb
val make_dp : int array -> comb
val make_brelan : int array -> comb
val m : comb
val make_p : int array -> comb
val make_h_c : int array -> carte list -> carte list -> comb
val make_c : carte list -> comb
val make_ch : int array -> comb
exception Others_exception
val test_qf : comb -> comb -> int
val test_full : comb -> comb -> int
val test_carre : comb -> comb -> int
val test_brelan : comb -> comb -> int
val test_dp : comb -> comb -> int
val test_p : comb -> comb -> int
val test_occ : int array -> int array -> int
val compare_col : carte list -> carte list -> int
val compare_equals_2 :
  int ->
  int array -> int array -> carte list -> carte list -> carte list -> int
val compare_hands : carte list -> carte list -> carte list -> int
val make_prob_4 : carte list -> carte list -> carte list -> float * float
val make_prob_3 : carte list -> carte list -> carte list -> float * float
val proba_double : carte list -> carte list -> carte list -> float * float
val proba_simple : carte list -> carte list -> float * float
