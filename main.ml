(* handle SIGINT *)
(* let _ = Sys. *)


let init_mine = Grid.parse ()

let _ = Printf.eprintf "Map parsed !\n\n"

let _ = print_string (Grid.to_string init_mine)
