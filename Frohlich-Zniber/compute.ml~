type couleur =
    Pique
  | Carreau
  | Coeur
  | Trefle
  | Vide
;;

type rang  =
  | Value of int
  | Valet
  | Dame
  | Roi
  | As
  | Vide
;;

type carte = 
| Carte of rang * couleur
;;

type donne =
  carte list
;;

type table =
  carte list
;;

type comb =
| Carte_haut of rang list
| Paire of rang * rang * rang * rang
| Double_Paire of rang * rang * rang
| Brelan of rang * rang *  rang
| Suite of rang
| Couleur of rang list
| Full of rang * rang
| Carre of rang * rang
| Quinte_flush of rang
;;

(* ------------- Conversion ------------------ *)
exception Rang_exception
exception Couleur_exception

(*pour convertir un rang a un int j'allais faire ça mais je me suis c pas possible 
j'ai meme pensé at genre As > Dame > Valet je c j avais testé ça aussi mais c pas le but ici  *)
let val_rang r = match r with
  | Value  e -> e
  | Valet -> 11
  | Dame -> 12
  | Roi -> 13
  | As -> 14
  | Vide -> raise Rang_exception
;;

let int_to_rang n =
  if n = 14 then As
  else if n = 13 then Roi
  else if n = 12 then Dame
  else if n = 11 then Valet
  else Value n
;;

let int_to_couleur n =
  if n = 1 then Trefle
  else if n = 2 then Pique
  else if n = 3 then Coeur
  else if n = 4 then Carreau
  else raise Couleur_exception
;;

let couleur_to_int c =
  if c = Trefle then 1
  else if c = Pique then 2
  else if c = Coeur then 3
  else 4
;;

let string_of_col c =
  if c = 1 then "T"
  else if c = 2 then "P"
  else if c = 3 then "Co"
  else "Ca"
;;

let string_of_rang  r =
  if r <= 10 then string_of_int r
  else if r = 11 then "V"
  else if r = 12 then "D"
  else if r = 13 then "R"
  else "A"
;;

let string_of_carte c =
  match c with
  | Carte(r,cc) -> (string_of_rang(val_rang r))^(string_of_col(couleur_to_int cc))
;;

(* ------------------------------- *)

(* incremente la valeur de chaque elt trouver *)
let modifier_tab tab r =
  tab.(val_rang r) <- tab.(val_rang r) + 1;
  tab
;;

(* la table d occurence a partir d'unne donne et une tabl ça je l'ai faits *)
let occ d t =
  let li = t@d in
  let tab = Array.make 15 0 in
  let rec aux l table = match l with
      [] -> table
    | Carte(r,c)::q -> aux q (modifier_tab table r)
  in aux li tab;
;;


(* ------------------- verification du combo -----------------------------*)

(* la combine du joueur est un CARRE*)
(* l'argument ici est la tableau d'occ *)
(* l'arg tab c'est a chaque fois la donne + la table *)
let is_Carre tab =
  let l = Array.to_list tab in
  List.mem 4 l
;;


(* 3 occ d'une  carte et tt les autre ya que une occ *)
let is_Brelan tab =
  let l = Array.to_list tab in
  List.mem 3 l
;;

(* 3 occ de la mm carte plus 2 occ d'un autre *)
let is_Full tab =
  let li = Array.to_list tab in
  let rec aux l n = match l with
      [] -> n
    | a::q -> if a = 3 then aux q (n+1) else aux q n
  in
  let res = aux li 0 in
  (List.mem 3 li && List.mem 2 li) || (res = 2)
;;

(* on a 2 qq part et une seul fois dans la table d occ *)
let is_Paire tab =
  let l = Array.to_list tab in
  List.mem 2 l
;;

(* je verifier si dans la table d'occ ya deux carte 2 fois *)
let is_Double_Paire tab =
  let li = Array.to_list tab in
  let rec aux l n = match l with
      [] -> n
    | a::q -> if a = 2 then aux q (n+1) else aux q n
  in
  let res = aux li 0 in (res >= 2)
  (* si jamais on a 2;2;2;...;1;.. ou autre *)
;;

(* ya une suite avec un as au debut*)
let as_one tab =
  (tab.(2) >= 1 && tab.(3) >= 1 && tab.(4) >= 1 && tab.(5) >= 1 && tab.(14) >= 1)
;;

(* cette fonction est utiliser pour les suite et les q_f *)
let indice_last tab =
  if (as_one tab) then 5
  else
    begin
      let l = Array.to_list tab in
      let rec aux l i n = match l with
	  [] -> (i-1)
	| a::q -> if n >= 5 && a = 0 then (i-1) else if a >= 1 then aux q (i+1) (n+1) else aux q (i+1) 0
      in aux l 0 0
    end
;;


(* si on 5 "1" qui se suivent dans notre table d'occ c'est qu'on a une suite *)
(* "a>0" pour le cas la ou oui on a ..;3;1;1;1;1;0.. est aussi une suite *)
let is_Suite tab =
  let li = Array.to_list tab in
  let rec aux l m = match l with
      [] -> m
    | a::q ->
       if m = 5 then m else if a >= 1 then aux q (m+1) else aux q 0
  in
  let res = aux li 0 in (as_one tab || res >= 5)
;;

indice_last [|0;0;1;1;0;0;0;1;1;1;1;1;1;1;0|];;

(* on verifie couleur par couleur *)
let is_Col l c = 
  let rec aux li n col = match li with
      [] -> n
    | Carte(r,c)::q ->
       if c = col then aux q (n+1) col else aux q n col
  in aux l 0 c 
;;

(* l = d@t *)
(* c trivial quoi.. *)
let is_Couleur l =
  (is_Col l Pique >= 5) || (is_Col l Trefle >= 5) ||
    (is_Col l Carreau >=5) || (is_Col l Coeur >= 5)

;;

let res_couleur l =
  if (is_Col l Pique) >= 5 then Pique
  else if (is_Col l Trefle) >= 5 then Trefle
  else if (is_Col l Coeur) >= 5 then Coeur
  else Carreau
;;
 
(* l = d@t *)
let is_Quinte_flush tab d t =
  let l = d@t in
  if (is_Suite tab) then
    if (is_Couleur l ) then
      begin
	let col = res_couleur l in
	let rec aux li li' = match li with
	    [] -> li'
	  | Carte(r,c)::q -> if c = col then aux q (Carte(r,c)::li') else aux q li'
	in
	let l_col = aux l [] in
	(is_Suite (occ l_col [] ) )	      
      end
    else false
  else false
;;

(* ------------------------- fin des verification -------------------------------*)

let valeur_best_comb tab d t =
  if(is_Quinte_flush tab d t ) then 9
  else if (is_Carre tab ) then 8
  else if (is_Full  tab ) then 7
  else if (is_Couleur (d@t) ) then 6
  else if (is_Suite tab) then 5
  else if (is_Brelan tab) then 4
  else if (is_Double_Paire tab) then 3
  else if (is_Paire tab) then 2
  else (* carte haute *) 1
;;

(* le premier dernier 1 a trouver *)
(* la pos du carte  par ordre *)
(* pour le dernier o est a 0 av dern est a 1 etc.. *)
let indice_carte tab carte o =
  let rec aux t n c o1 acc =
    if tab.(n) = c && o1 = acc then n
    else if tab.(n) = c && o1 <> acc then aux tab (n-1) c o1 (acc+1)
    else aux tab (n-1) c o1 acc
  in aux tab ((Array.length tab)-1) carte o 0
;;

(* renvoie l indice du dernier element par ordre lexico *)
let carre_b tab n =
  let indice = ref 0 in
  let cpt = ref 0 in
  for i = (Array.length tab)-1 downto 0 do
    if (tab.(i) <> 0 && i <> n && !cpt = 0) then
      begin
	indice := i;
	cpt := !cpt + 1
      end
  done;
  !indice
;;

let exist_3 tab m   =
  let b = ref false in
  for i = (Array.length tab)-1 downto 0 do
    if i <> m && tab.(i) = 3 then b := true
  done;
  !b
;;

(* verifier si il y a un deuxieme 3 dans la table d occ*)
let full_b tab n =
  let bool = exist_3 tab n in
  let res = if bool then 3 else 2 in
  let indice = ref 0 in
  let cpt = ref 0 in
  for i = (Array.length tab)-1 downto 0 do
    if (tab.(i) = res && i <> n && !cpt = 0) then
      begin
	indice := i;
	cpt := !cpt + 1
      end
  done;
  !indice
;;

(* verification du double paire pour recuperer le denier indice par ordre lexico *)
let dp_b tab n m =
  let indice = ref 0 in
  let cpt = ref 0 in
  for i = (Array.length tab)-1 downto 0 do
    if (tab.(i) <> 0 && i <> n && i <> m && !cpt = 0) then
      begin
	indice := i;
	cpt := !cpt + 1
      end
  done;
  !indice
;;

(* --------------------------------- make combine ----------------------------- *)

let make_qf  d t =
  let couleur = res_couleur (d@t) in
  let rec aux l l' col = match l with
      [] -> l'
    | Carte(r,c)::q -> if c = col then aux q (Carte(r,c)::l') col else aux q l' col
  in
  let tab = occ (aux (d@t) [] couleur) [] in
  let r = indice_last tab in Quinte_flush (int_to_rang r)
;;

let make_carre tab =
  let x1 = indice_carte tab 4 0 in
  let x2 = carre_b tab x1 in
  Carre (int_to_rang x1,int_to_rang x2)
;;

let t = make_carre [|0;0;0;0;0;0;2;0;0;4;0;1;0;0;0|];;

let make_full tab =
  let x1 = indice_carte tab 3 0 in
  let x2 = full_b tab x1 in
  Full (int_to_rang x1, int_to_rang x2)
;;

let t = make_full [|0;0;0;0;1;0;2;0;0;3;0;1;0;0;0|];;
let m = full_b     [|0;0;0;0;1;0;2;0;0;3;0;1;0;0;0|] 9;;


let make_suite tab =
  let x = indice_last tab in Suite(int_to_rang x)
;;

let m = make_suite  [|0;0;0;0;1;0;2;0;1;1;1;1;1;0;0|] ;;

let make_dp tab =
  let x1 = indice_carte tab 2 0 in
  let y1 = indice_carte tab 2 1 in      
  let z1 = dp_b tab x1 y1 in
  Double_Paire (int_to_rang x1, int_to_rang y1, int_to_rang z1)
;;

let m = make_dp  [|0;0;0;0;1;0;2;0;1;1;0;2;0;0;0|] ;;


let make_brelan tab =
  let x1 = int_to_rang (indice_carte tab 3 0) in
  let x2 = int_to_rang (indice_carte tab 1 0) in
  let x3 = int_to_rang (indice_carte tab 1 1) in
  Brelan ( x1, x2, x3)
;;

let m = make_brelan  [|0;0;0;0;1;0;3;0;0;1;0;1;1;0;0|] ;;


let make_p tab =
  let x1 = int_to_rang (indice_carte tab 2 0) in
  let y1 = int_to_rang (indice_carte tab 1 0) in
  let z1 = int_to_rang (indice_carte tab 1 1) in
  let a1 = int_to_rang (indice_carte tab 1 2) in
  Paire ( x1, y1,  z1, a1)
;;

(* make carte haut et make couleur *)
let make_h_c tab d t =
  if (is_Couleur (d@t)) then
    begin
      let couleur = res_couleur (d@t) in
      let rec aux l l' col = match l with
	  [] -> l'
	| Carte(r,c)::q -> if c = col then aux q (r::l') col else aux q l' col
      in Couleur (aux (d@t) [] couleur)
    end
  else
    let x1 = int_to_rang (indice_carte tab 1 0) in
    let x2 = int_to_rang (indice_carte tab 1 1) in
    let x3 = int_to_rang (indice_carte tab 1 2) in
    let x4 = int_to_rang (indice_carte tab 1 3) in
    let x5 = int_to_rang (indice_carte tab 1 4) in
    Carte_haut [x1;x2;x3;x4;x5]
;;


let make_c l = 
  let couleur = res_couleur l in
  let rec aux l l' col = match l with
      [] -> l'
    | Carte(r,c)::q -> if c = col then aux q (r::l') col else aux q l' col
  in Couleur (aux l [] couleur)
;;

let make_ch tab = 
  let x1 = int_to_rang (indice_carte tab 1 0) in
  let x2 = int_to_rang (indice_carte tab 1 1) in
  let x3 = int_to_rang (indice_carte tab 1 2) in
  let x4 = int_to_rang (indice_carte tab 1 3) in
  let x5 = int_to_rang (indice_carte tab 1 4) in
  Carte_haut [x1;x2;x3;x4;x5]
;;


(* -------------------------- fin make combine --------------------------- *)


(* ------------------------- comparaison d egalité ------------------------*)

(* c la mm chose que ompare equals *)

exception Others_exception

let test_qf c1 c2 = match (c1,c2) with
    
  | Quinte_flush(r1) , Quinte_flush(r2) -> if (val_rang r1) > (val_rang r2 ) then 1
    else if (val_rang r1) < (val_rang r2 ) then -1 else 0
  |   _ -> raise Others_exception

;;

let test_full c1 c2 = match c1,c2 with
  | Full(r1,r2) , Full(o1,o2) ->
     if (val_rang r1) > (val_rang o1 ) then 1
     else if (val_rang r1) < (val_rang o1 ) then -1
     else if (val_rang r2) > (val_rang o2 ) then 1
     else if (val_rang r2) < (val_rang o2 ) then -1
     else 0
  |  _ -> raise Others_exception
     
;;


let test_carre c1 c2 = match c1,c2 with
  | Carre(r1,r2) , Carre(o1,o2) ->
     if (val_rang r1) > (val_rang o1 ) then 1
     else if (val_rang r1) < (val_rang o1 ) then -1
     else if (val_rang r2) > (val_rang o2 ) then 1
     else if (val_rang r2) < (val_rang o2 ) then -1
     else 0
  |  _ -> raise Others_exception
     
;;

let test_brelan c1 c2 = match c1,c2 with
  | Brelan(r1,r2,r3) , Brelan(o1,o2,o3) ->
     if (val_rang r1) > (val_rang o1 ) then 1
     else if (val_rang r1) < (val_rang o1 ) then -1
     else if (val_rang r2) > (val_rang o2 ) then 1
     else if (val_rang r2) < (val_rang o2 ) then -1
     else if (val_rang r3) > (val_rang o3 ) then 1
     else if (val_rang r3) < (val_rang o3 ) then -1
     else 0
  | _ -> raise Others_exception
;;

let test_dp c1 c2 = match c1,c2 with
  |Double_Paire(r1,r2,r3) , Double_Paire(o1,o2,o3) ->
     if (val_rang r1) > (val_rang o1 ) then 1
     else if (val_rang r1) < (val_rang o1 ) then -1
     else if (val_rang r2) > (val_rang o2 ) then 1
     else if (val_rang r2) < (val_rang o2 ) then -1
     else if (val_rang r3) > (val_rang o3 ) then 1
     else if (val_rang r3) < (val_rang o3 ) then -1
     else 0
  | _ -> raise Others_exception
;;

let test_p c1 c2 = match c1,c2 with
  |Paire(r1,r2,r3,r4) , Paire(o1,o2,o3,o4) ->
     if (val_rang r1) > (val_rang o1 ) then 1
     else if (val_rang r1) < (val_rang o1 ) then -1
     else if (val_rang r2) > (val_rang o2 ) then 1
     else if (val_rang r2) < (val_rang o2 ) then -1
     else if (val_rang r3) > (val_rang o3 ) then 1
     else if (val_rang r3) < (val_rang o3 ) then -1
     else if (val_rang r4) > (val_rang o4 ) then 1
     else if (val_rang r4) < (val_rang o4 ) then -1
     else 0
  | _ -> raise Others_exception
;;

let test_occ occ1 occ2 =
  let x1 = indice_carte occ1 1 0 in
  let x2 = indice_carte occ1 1 1 in
  let x3 = indice_carte occ1 1 2 in
  let x4 = indice_carte occ1 1 3 in
  let x5 = indice_carte occ1 1 4 in
  
  let y1 = indice_carte occ2 1 0 in
  let y2 = indice_carte occ2 1 1 in
  let y3 = indice_carte occ2 1 2 in
  let y4 = indice_carte occ2 1 3 in
  let y5 = indice_carte occ2 1 4 in
  
  if x1 > y1 then 1
  else if x1 < y1 then -1
  else if x2 > y2 then 1
  else if x2 < y2 then -1
  else if x3 > y3 then 1
  else if x3 < y3 then -1
  else if x4 > y4 then 1
  else if x4 < y4 then -1
  else if x5 > y5 then 1
  else if x5 < y5 then -1
  else 0
;;

let compare_col  l1 l2 =
  let couleur_list = res_couleur l1 in 
  let rec aux l l' = match l with
      [] -> l'
    | Carte(r,c)::q -> if c = couleur_list then aux q (Carte(r,c)::l') else aux q l'
  in
  let tab1 = occ (aux l1 []) [] in
  let tab2 = occ (aux l2 []) [] in
  test_occ tab1 tab2
;;
	
let compare_equals_2 j occ1 occ2 d1 d2 t =
  if j = 9 then (* qf *)
    begin
      let comb1 = make_qf  d1 t in
      let comb2 = make_qf  d2 t in
      test_qf comb1 comb2
    end
  else if j = 8 then (* carre *)
    begin
      let comb1 = make_carre occ1 in
      let comb2 = make_carre occ2 in
      test_carre comb1 comb2
    end
  else if j = 5 then (* suite *)
    begin
      let x1 = indice_last occ1  in
      let x2 = indice_last occ2  in
      if x1 > x2 then 1 else if x1 < x2 then -1 else 0
    end
  else if j = 7 then (* full *)
    begin
      let comb1 = make_full occ1 in  
      let comb2 = make_full occ2 in
      test_full comb1 comb2
    end
  else if j = 4 then (* Brelan *)
    begin
      let comb1 = make_brelan occ1 in
      let comb2 = make_brelan occ2 in
      test_brelan comb1 comb2
    end
  else if j = 3 then (* dp *)
     begin
      let comb1 = make_dp occ1 in
      let comb2 = make_dp occ2 in
      test_dp comb1 comb2 
    end
  else if j = 2 then (* p *)
     begin
      let comb1 = make_p occ1 in
      let comb2 = make_p occ2 in
      test_p comb1 comb2
     end
  else if j = 6  then (* couleur  *)
    compare_col (d1@t) (d2@t)
  else if j = 1 then  (* c_h *)
    test_occ occ1 occ2
  else 0
;;

(* ------------------------- fin de comparaison d egalité ------------------*)

let compare_hands d1 d2 t =
  let occ1 = occ d1 t in
  let occ2 = occ d2 t in
  let joueur1 = valeur_best_comb occ1 d1 t in
  let joueur2 = valeur_best_comb occ2 d2 t in
  if joueur1 > joueur2 then 1
  else if joueur1 < joueur2 then -1
  else  compare_equals_2 joueur1 occ1 occ2 d1 d2 t
;;


(* proba_double quand ya 4 carte en table *)
let make_prob_4 d1 d2 t =
  let li = d1@d2@t in
  let cpt_d1 = ref 0. in
  let cpt_d2 = ref 0. in
  for i = 2 to 14 do
    for j = 1 to 4 do
      let c = Carte(int_to_rang i, int_to_couleur j) in
      if (List.mem c li) = false  then
	begin
	  let lt = c::t in
	  let res = compare_hands d1 d2 lt in	  
	  if res = 1 then cpt_d1 := 1. +. !cpt_d1
	  else if res = -1 then cpt_d2 := 1. +. !cpt_d2	  	   
	end           
    done;
  done;
  (*let size = float_of_int (List.length !list_f) in*)
  (!cpt_d1 /. 44. ),(!cpt_d2 /. 44. )(*,(!list_f)*)
;;

(* proba_double quand ya 3 carte en table *)
let make_prob_3 d1 d2 t =
  let li = d1@d2@t in
  let cpt_d1 = ref 0. in
  let cpt_d2 = ref 0. in
  for i = 2 to 14 do
    for j = 1 to 4 do
      let c = Carte(int_to_rang i, int_to_couleur j) in
      if (List.mem c li) = false (*&& exit_in_l d1 d2 (c::t) list_ f*) then	
	begin
	  let lt = c::t in
	  let res = make_prob_4 d1 d2 lt in
	  match res with
	  |(f1,f2) -> cpt_d1 := !cpt_d1 +. f1 ; cpt_d2 := !cpt_d2 +. f2; 
	end    
    done;
  done;
  (!cpt_d1 /. 45.),(!cpt_d2 /. 45.)
;;

let proba_double d1 d2 t =
  let len = List.length t in
  if len = 5 then
    begin
      let res = compare_hands d1 d2 t in
      if res = 1 then (1.,0.)
      else if res = -1 then (0.,1.)
      else (0.,0.)      
    end
  else if len = 4 then 
    make_prob_4 d1 d2 t
  else
    make_prob_3 d1 d2 t
;;

let proba_simple d1 t =
  let li = d1@t in
  let cpt_d1 = ref 0. in
  let cpt_d2 = ref 0. in
  
  for i = 2 to 14 do
    for j = 1 to 4 do
      (* premiere carte de la donne adverse *)
      let c = Carte(int_to_rang i, int_to_couleur j) in
      if (List.mem c li) = false then	
	begin
	  let d2_1 = c::[] in (* la donne adverse avec une seul carte *)
	  let li2 = d1@d2_1@t in
	  for m = 2 to 14 do
	    for n = 1 to 4 do
	      let c2 = Carte(int_to_rang m, int_to_couleur n) in
	      if (List.mem c2 li2) = false then
		
		begin
		  let d2_2 = c2::d2_1 in (* la donne adverse avec deux cartes *)
		  let res = proba_double d1 d2_2 t in 
		  match res with
		  | (f1,f2) -> cpt_d1 := !cpt_d1 +. f1; cpt_d2 := !cpt_d2 +. f2	  
		end    
	    done;
	  done;
	end
    done;
  done;
  
  let x1 = 44. *. 45. in
  let x2 = 45. *. 46. in
  let x3 = 46. *. 47. in

  if      (List.length t) = 3 then (!cpt_d1 /. x3),(!cpt_d2 /. x3)
  else if (List.length t) = 4 then (!cpt_d1 /. x2),(!cpt_d2 /. x2)
  else (*if (List.length t) = 5 then*) (!cpt_d1 /. x1),(!cpt_d2 /. x1)

;;
