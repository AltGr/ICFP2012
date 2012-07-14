(* handle SIGINT *)
(* let _ = Sys. *)

open Grid.T
open Moves.T

let init_mine =
  if Array.length Sys.argv > 1 then
    let f = open_in Sys.argv.(1) in
    let g = Grid.parse f in
    close_in f;
    g
  else
    Grid.parse stdin

let rec loop mine =
  print_string (Grid.to_color_string mine);
  Printf.printf "(Score:%d) Move> " mine.score;
  flush stdout;
  let move =
    try Scanf.scanf "%c\n" Moves.char_to_move with
    | Invalid_argument _ -> Wait
    | _ -> Abort
  in
  let move = if Moves.is_valid mine move then move else Wait in
  let mine =
    try Moves.apply mine move
    with
    | Won ->
      let score = mine.score + mine.collected * 50 in
      Printf.eprintf "Victory ! Score: %d\n" score;
      exit 0
    | Aborted ->
      let score = mine.score + mine.collected * 25 in
      Printf.eprintf "Aborted ; Score: %d\n" score;
      exit 0
  in
  let mine =
    try Update.update { mine with score = pred (mine.score) }
    with
    | Update.Dead ->
      Printf.eprintf "You're dead ! Score: %d\n" mine.score;
      exit 0
  in
  loop mine

let _ = loop init_mine
