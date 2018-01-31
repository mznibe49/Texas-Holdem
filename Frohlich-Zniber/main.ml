open Graphics
open Compute
open Readfilef
open Gui

let main () =
  try
    if Sys.file_exists (Sys.argv.(1)) then draw_from_file (Sys.argv.(1))
    else print_string "Le fichier n'a pas été trouvé.\nAlors voici votre version d'Ocaml pour vous consoler: "; print_string Sys.ocaml_version ; print_newline (); 
  with 
  | Graphic_failure "fatal I/O error" -> print_string "Bye bye." ; print_newline ()
  | Format_error -> print_string "Mauvais format de fichier." ; print_newline ()
  | Invalid_argument "index out of bounds" ->
    try 
      draw ();
    with
    | Graphic_failure "fatal I/O error" -> print_string "Bye bye." ; print_newline ()
;; 

main ();;
