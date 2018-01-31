val string_to_rang : string -> Compute.rang
val string_to_couleur : string -> Compute.couleur
exception Format_error
val create_card : string -> Compute.carte
val create_card_list : string -> Compute.carte list
val read_file : string -> string list
val test_duplicate : 'a list -> bool
val test_duplicate_from_list : 'a list -> 'a list -> bool
val create_game_from_file : string -> Compute.carte list list
val cut_float : float -> float
val string_of_ff : float * float -> string
val string_of_ch : int -> string
val compute : string -> string
val test_rapide : unit -> unit
