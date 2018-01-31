open Compute
open Readfilef
open Graphics

(* Créé un deck, pour l'affichage et les actions liés à celui-ci (re-ajout de carte, suppression de carte, selection) *)
let init_board () = 
  let x = ref 10 in
  let y = ref 440 in

  let tab = Array.make 52 (Carte(Vide,Vide),0,0) in
  let var = ref 0 in
  let i = ref 2 in
  let j = ref 1 in

  while (!var < 52) do
    tab.(!var) <- (Carte(int_to_rang !i, int_to_couleur !j),!x,!y);
    j := !j +1;
    var := !var + 1;
    y := !y - 60;
    if !j > 4
    then
      begin
	j := 1;
	i := !i + 1;
	x := !x + 40;
	y := 440
      end
  done;
  tab
;;

(* Dessine une carte blanche sans nom *)
let draw_card x y  =  
  set_color white;
  fill_rect x y 30 50
;;

(* Dessine une donne ou table ou autre (n = le nombre de carte) en x y*)
let draw_d x y n =
  let tab = Array.make n (Carte(Vide,Vide),0,0) in
  let k = ref x in
  for i = 0 to n-1 do
    tab.(i) <- (Carte(Vide,Vide),!k,y);
    set_color (rgb 39 39 39);
    fill_rect (!k+3) (y-3) 30 50;
    draw_card !k y;
    k := !k + 40; 
  done;
  tab
;;

let draw_reset x y =
  set_color (rgb 36 37 176);
  fill_rect (x+3) (y-3) 50 30;
  set_color white;
  fill_rect x y 50 30;
  moveto (x+5) (y+5);
  set_color (rgb 36 37 176);
  draw_rect x y 50 30;
  draw_string "Reload";
;;

(* Renvoie la carte cliquée et sa position *)
let is_clicked t mx my =
  let card = ref (Carte(Vide,Vide)) in
  let cx = ref 0 and cy = ref 0 in
  for i = 0 to (Array.length t)-1 do
    match t.(i) with
    |(c,x,y) ->
       if (mx >= x && mx <= x+30) && (my >= y && my <= y+50)
       then
	 begin
	   cx := x;
	   cy := y;
	   card := c;	   
	 end
  done;
  !card,!cx,!cy
;;

let array_mem t c =
  let li = Array.to_list t in (List.mem c li)
;;

(* Mets la carte c à Carte(Vide,Vide) dans t *)
let change_to_empty t c =
  let taille = (Array.length t)-1 in
  for i = 0 to taille do
    match t.(i) with
    | (ca,x,y) -> if ca = c then t.(i) <- (Carte(Vide,Vide),x,y);
  done;
;;

(* Modifie dans le deck la carte c (grise si déja prise, blanche sinon) *)
let display_deck_changes b c col =
  let taille = (Array.length b) - 1 in
  for i = 0 to taille do
    match b.(i) with
    | (ca,x,y) -> 
      if ca = c 
      then 
	begin 
	  match ca with
	  | Carte(r,c) -> 
	    begin
	      if col then
		begin
		  set_color white;
		  fill_rect x y 30 50;
		  moveto (x+5) (y+5);
		  if couleur_to_int c < 3 then set_color black else set_color red;
		  draw_string ((string_of_rang (val_rang r))^(string_of_col (couleur_to_int (c))));
		end
	      else if col = false then
		begin
		  set_color (rgb 145 145 145);
		  fill_rect x y 30 50;
		  moveto (x+5) (y+5);
		  if couleur_to_int c < 3 then set_color black else set_color red;
		  draw_string ((string_of_rang (val_rang r))^(string_of_col (couleur_to_int (c))));
		end
	    end
	end
  done;
;;

(* Affiche la carte du deck encadré rouge si elle est selectionnée *)
let display_selection_in_deck b c select =
  for i = 0 to (Array.length b) - 1 do
    match b.(i) with
    | (cb,x,y) -> 
      if cb = c 
      then
	begin
	  if select then 
	    begin 
	      set_color red; 
	      draw_rect x y 30 50; 
	    end
	  else 
	    begin 
	      set_color white; 
	      draw_rect x y 30 50;
	    end
	end
  done;
;;

(* Dessine le nom de la carte cliquée selon la carte courante *)
let draw_card_name c cc =
  match c with
  | (_,x,y) -> 
    match cc with 
    | Carte(r,co) -> 
      if couleur_to_int co > 2 then begin moveto (x+5) (y+5); set_color red; draw_string (string_of_carte cc) end
      else begin moveto (x+5) (y+5); set_color black; draw_string (string_of_carte cc) end
;;

let print_triplet ct =
  match ct with 
  | (c,x,y) -> moveto 5 5; set_color red; fill_rect 5 5 30 10; set_color white; draw_string ( (string_of_carte (c)));
;;

(* Non utilisée, mais utile pour debuggage *)
let print_tab_triplet t =
  for i = 0 to (Array.length t)-1 do
    match t.(i) with
    | (c,x,y) -> print_string ("[Carte: "^(string_of_carte c)^"] ")
  done;
;;
	  
let add_triplet t trip =
  let taille = (Array.length t) - 1 in
  for i = 0 to taille do
    match t.(i) with
    | (c,x,y) -> 
      match trip with
      | (ct,xt,yt) -> 
	if x = xt && y = yt then t.(i) <- trip
  done;
;;

(* Place dans le deck la carte associée dans le modèle de deck b *)
let put_in_deck b deck c =
  for i = 0 to (Array.length deck) - 1 do
    match b.(i) with
    | (cb,x,y) -> 
      if cb = c then deck.(i) <- (c,x,y)
  done;
;;

(* Renvoie le nombre de carte non vide du tableau *)
let count_not_empty t =
  let cpt = ref 0 in
  for i = 0 to (Array.length t) - 1 do
    match t.(i) with
    | (c,x,y) -> if c <> Carte(Vide,Vide) then cpt := !cpt + 1
  done;
  !cpt
;;

(* Renvoi la liste des cartes non vides de t *)
let extract_card_from_t t =
  let l = ref [] in
  for i = 0 to (Array.length t) - 1 do
    match t.(i) with
    | (c,x,y) -> if c <> Carte(Vide,Vide) then l := !l@[c]
  done;
  !l
;;

let draw_scoreboard s =
  set_color white;
  fill_rect 185 105 160 40;
  set_color red;
  draw_rect 185 105 160 40;
  moveto 188 120;
  draw_string s;
;;

let draw_chronometer () =
  set_color black;
  fill_rect 10 45 40 15; 
  moveto 12 45; 
  set_color white
;;

let draw_compute d1 d2 t =
  if count_not_empty d2 = 2 && count_not_empty t < 5
  then
    begin 
      let beginning = Sys.time() in
      let () = draw_scoreboard (string_of_ff (proba_double (extract_card_from_t d1) (extract_card_from_t d2)  (extract_card_from_t t))) in
      let ending = Sys.time() in
      let time = ending -. beginning in
      let () = draw_chronometer () in
      draw_string (string_of_float time);
    end
  else if count_not_empty d2 = 2 && count_not_empty t = 5
  then
    let beginning = Sys.time() in
    let () = draw_scoreboard (string_of_ch (compare_hands (extract_card_from_t d1) (extract_card_from_t d2) (extract_card_from_t t))) in
    let ending = Sys.time() in
    let time = ending -. beginning in
    let () = draw_chronometer () in
    draw_string (string_of_float time);
  else 
    begin
      draw_chronometer ();
      draw_string "-";
      draw_scoreboard "[Calculs en cours........]";
      let beginning = Sys.time() in
      let () = draw_scoreboard (string_of_ff (proba_simple (extract_card_from_t d1) (extract_card_from_t t))) in
      let ending = Sys.time() in
      let time = ending -. beginning in
      let () = draw_chronometer () in
      draw_string (string_of_float time);
    end 
;; 

(* Dessine les changements selon les actions sur les cartes  *)
let display_changes t c current_card carte_c b deck x y =
  if current_card <> Carte(Vide,Vide) && (array_mem t (Carte(Vide,Vide),x,y))
  then
    begin
      draw_card x y;
      draw_card_name c current_card;
      display_deck_changes b current_card false;
      add_triplet t (current_card,x,y);
      change_to_empty deck current_card
    end
  else if current_card <> Carte(Vide,Vide) && (array_mem t (Carte(Vide,Vide),x,y)) = false
  then
    begin
      put_in_deck b deck carte_c;
      change_to_empty deck current_card;
      add_triplet t (current_card,x,y);
      draw_card x y;
      draw_card_name c current_card;
      display_deck_changes b carte_c true;
      display_deck_changes b current_card false;
    end
  else if current_card = Carte(Vide,Vide) && (array_mem t (Carte(Vide,Vide),x,y)) = false
  then
    begin
      put_in_deck b deck carte_c;
      change_to_empty t carte_c;
      draw_card x y;
      display_deck_changes b carte_c true;
    end
;;

(* Dessine les donnes, la table et le deck selon le fichier chargé precedemment *)
let display_from_file d1 d2 t b =
  let l = Array.concat [d1;d2;t] in
  for i = 0 to (Array.length l) - 1 do
    match l.(i) with
    | (Carte(r,c),x,y) ->
      begin
	if Carte(r,c) <> Carte(Vide,Vide) then 
	  begin 
	    draw_card_name (Carte(r,c),x,y) (Carte(r,c));
	    display_deck_changes b (Carte(r,c)) false;
	    change_to_empty b (Carte(r,c))
	  end
      end
  done;
;;

(* Simule une "partie" de Texas Hold'em entre deux joueurs et fait les calculs associés *)
let game d1 d2 t b from_file =
  let deck = ref (Array.copy b) in
  let () = if from_file then display_from_file d1 d2 t !deck in
  let donne_1 = ref d1 in
  let donne_2 = ref d2 in
  let table = ref t in
  let current_card = ref (Carte(Vide,Vide)) in
  let end_of_the_world = ref true in
  while !end_of_the_world do
    if (count_not_empty !donne_1 = 2 && (count_not_empty !donne_2 = 0 || count_not_empty !donne_2 = 2) && count_not_empty !table > 2)
    then 
      draw_compute !donne_1 !donne_2 !table
    else 
      begin 
	draw_chronometer ();
	draw_string "-";
	draw_scoreboard "[En attente de calculs...]";
      end;
    let e = wait_next_event[Button_down] in
    if e.button then
      begin
	let mx = e.mouse_x in
	let my = e.mouse_y in
	let () = 
	  if mx >= 10 && mx <= 60 && my >= 10 && my <= 40
	  then begin sound 1 1000; end_of_the_world := false; end
	in
	let all = Array.concat [d1;d2;t;b] in 
	let c = is_clicked all mx my in
	match c with 
	| (carte_c,x,y) -> 
	  if (array_mem !deck c) && carte_c <> Carte(Vide,Vide)
	  then 
	    begin
	      if carte_c = !current_card 
	      then 
		begin
		  display_selection_in_deck b !current_card false;
		  current_card := Carte(Vide,Vide)
		end
	      else 
		begin 
		  display_selection_in_deck b !current_card false;
		  display_selection_in_deck b carte_c true;
		  current_card := carte_c;
		end
	    end
	  else if (array_mem d1 c)
	  then 
	    begin
	      display_changes !donne_1 c !current_card carte_c b !deck x y;
	      current_card := Carte(Vide,Vide);
	    end
	  else if (array_mem d2 c)
	  then 
	    begin
	      display_changes !donne_2 c !current_card carte_c b !deck x y;
	      current_card := Carte(Vide,Vide);
	    end 
	  else if (array_mem t c)
	  then 
	    begin
	      display_changes !table c !current_card carte_c b !deck x y;
	      current_card := Carte(Vide,Vide);
	    end
      end
  done;
;;

let is_Empty t =
  let l = (Array.to_list t) in 
  List.exists (fun x -> x = true) (List.map (fun (c,x,y) -> c = Carte(Vide,Vide)) l) 
;;

(* Dessine la fenêtre et le deck *)
let draw_window () =
  open_graph(" 530x500");
  set_window_title("Calculatrice de Texas Hold'Em");
  let () = 
    set_color (rgb 17 144 80);
    fill_rect 0 0 530 500;
    draw_reset 10 10;
    draw_chronometer ();
    moveto 12 45;
    draw_string "-"
  in
  let tab = init_board() in 
  let posX = ref 15 in
  let posY = ref 445 in
  let var = ref 0 in
  let j = ref 1 in
  let i = ref 2 in 
  while (!var < 52) do
    match tab.(!var) with
    | (m,n,o) ->
      set_color (rgb 39 39 39);
      fill_rect (n+3) (o-3) 30 50;
      draw_card n o;
      moveto !posX !posY;
      if !j < 3 then set_color black else set_color red;
      draw_string ((string_of_rang !i)^(string_of_col !j));      
      posY := !posY - 60; 
      j := !j +1;
      var := !var + 1;
      if !j > 4
      then
	begin
	  j := 1;
	  i := !i + 1;
	  posY := 445;
	  posX := !posX + 40
	end
  done;
  tab
;;

(* Dessine le tout si aucun fichier chargé *)
let rec draw () =
  let tab = draw_window () in
  let () =
    set_color (rgb 255 228 117);
    draw_rect 146 174 80 60;
    draw_rect 306 174 80 60;
    moveto 229 172;
    draw_string "D1";
    moveto 389 172;
    draw_string "D2";
    
    set_color white;
    draw_rect 166 14 200 60;
    moveto 369 12;
    draw_string "Table";
  in
  let d1 = draw_d 150 180 2 in
  let d2 = draw_d 310 180 2 in
  let table = draw_d 170 20 5 in
  while true do
    game d1 d2 table tab false;
    draw ()
  done;
;;   

(* Change le tableau t selon tf qui est chargé depuis un fichier  *)
let change_d_with_file t tf =
  try
    for i = 0 to (Array.length tf) - 1 do
      match tf.(i) with
      | c -> 
	match t.(i) with 
	  (ct,xt,yt) -> t.(i) <- (c,xt,yt)
    done;
    t
  with 
  | Invalid_argument "index out of bounds" -> raise Format_error
;;

(* Dessine la fenêtre selon le fichier f chargé *)
let draw_from_file f =
  let tab = draw_window () in
  let () = 
    set_color (rgb 255 228 117);
    draw_rect 146 174 80 60;
    draw_rect 306 174 80 60;
    moveto 229 172;
    draw_string "D1";
    moveto 389 172;
    draw_string "D2";
    
    set_color white;
    draw_rect 166 14 200 60;
    moveto 369 12;
    draw_string "Table";
  in
  let l = create_game_from_file f in
  let d1 = (change_d_with_file (draw_d 150 180 2) (Array.of_list (List.nth l 0))) in
  let d2 = (change_d_with_file (draw_d 310 180 2) (Array.of_list (List.nth l 1))) in
  let table = (change_d_with_file (draw_d 170 20 5) (Array.of_list (List.nth l 2))) in
  while true do
    game d1 d2 table tab true;
    draw ()
  done;
;;
