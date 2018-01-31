val init_board : unit -> (Compute.carte * int * int) array
val draw_card : int -> int -> unit
val draw_d : int -> int -> int -> (Compute.carte * int * int) array
val draw_reset : int -> int -> unit
val is_clicked :
  (Compute.carte * int * int) array ->
  int -> int -> Compute.carte * int * int
val array_mem : 'a array -> 'a -> bool
val change_to_empty :
  (Compute.carte * 'a * 'b) array -> Compute.carte -> unit
val display_deck_changes :
  (Compute.carte * int * int) array -> Compute.carte -> bool -> unit
val display_selection_in_deck : ('a * int * int) array -> 'a -> bool -> unit
val draw_card_name : 'a * int * int -> Compute.carte -> unit
val print_triplet : Compute.carte * 'a * 'b -> unit
val print_tab_triplet : (Compute.carte * 'a * 'b) array -> unit
val add_triplet : ('a * 'b * 'c) array -> 'a * 'b * 'c -> unit
val put_in_deck : ('a * 'b * 'c) array -> ('a * 'b * 'c) array -> 'a -> unit
val count_not_empty : (Compute.carte * 'a * 'b) array -> int
val extract_card_from_t :
  (Compute.carte * 'a * 'b) array -> Compute.carte list
val draw_scoreboard : string -> unit
val draw_chronometer : unit -> unit
val draw_compute :
  (Compute.carte * 'a * 'b) array ->
  (Compute.carte * 'c * 'd) array -> (Compute.carte * 'e * 'f) array -> unit
val display_changes :
  (Compute.carte * int * int) array ->
  'a * int * int ->
  Compute.carte ->
  Compute.carte ->
  (Compute.carte * int * int) array ->
  (Compute.carte * int * int) array -> int -> int -> unit
val display_from_file :
  (Compute.carte * int * int) array ->
  (Compute.carte * int * int) array ->
  (Compute.carte * int * int) array ->
  (Compute.carte * int * int) array -> unit
val game :
  (Compute.carte * int * int) array ->
  (Compute.carte * int * int) array ->
  (Compute.carte * int * int) array ->
  (Compute.carte * int * int) array -> bool -> unit
val is_Empty : (Compute.carte * 'a * 'b) array -> bool
val draw_window : unit -> (Compute.carte * int * int) array
val draw : unit -> unit
val change_d_with_file :
  ('a * 'b * 'c) array -> 'a array -> ('a * 'b * 'c) array
val draw_from_file : string -> unit
