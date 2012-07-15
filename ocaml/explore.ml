open Moves.T
open Grid.T

let all_moves = [Left; Right; Up; Down; Wait]

let possible_moves mine =
  List.filter (Moves.is_valid mine) all_moves

let rec list_filter_map f = function
  | [] -> []
  | x::r -> match f x with
    | Some y -> y :: list_filter_map f r
    | None -> list_filter_map f r

exception FoundWin of move list * int

let last_time_i_was_there = (Hashtbl.create 87 : (pos, mine) Hashtbl.t)

let rev_concat_map fct =
  let rec aux accu = function
    | [] -> accu
    | e::l -> aux (List.rev_append (fct e) accu) l
  in aux []


let reachable mine =
  let color = Array.make (Array.length mine.grid) (-1) in
  let len = mine.length in
  let max_moves = mine.length * mine.height - mine.moves in
  let rec aux i waterproof ((x,y) as pos) =
    if i > max_moves || waterproof < 0 then ()
    else if color.(y*len+x) < 0 then
      let waterproof =
        if y < mine.metadata.water then waterproof - 1 else mine.metadata.waterproof
      in
      let follow () =
        if x > 0 then aux (succ i) waterproof (x-1,y);
        if x < mine.length - 1 then aux (succ i) waterproof (x+1,y);
        if y > 0 then aux (succ i) waterproof (x,y-1);
        if y < mine.length - 1 then aux (succ i) waterproof (x,y+1)
      in
      match Grid.get mine pos with
      | Wall -> ()
      | Lambda | Earth | Robot | Empty ->
        color.(y*len+x) <- i;
        follow ()
      | Rock when (* Rock can move (approximatively) *)
          (y > 0 (* Rock can fall *)
           && (color.((y-1)*len+x) >= 0
               || match Grid.get mine (x,y-1) with
                 | Empty -> true
                 | Rock when
                     (x > 0
                      && (color.((y-1)*len+x-1) >= 0 || Grid.get mine (x-1,y-1) = Empty))
                     || (x < len-1
                         && (color.((y-1)*len+x+1) >= 0 || Grid.get mine (x+1,y-1) = Empty))
                     -> true
                 | Lambda when
                     x < len-1
                     && (color.((y-1)*len+x+1) >= 0 || Grid.get mine (x+1,y-1) = Empty)
                     -> true
                 | _ -> false))
          || (x>0 && x<len-1 (* Rock can be pushed *)
              && (
                (color.(y*len+x-1) >= 0 || Grid.get mine (x-1,y) = Empty)
                && (color.(y*len+x+1) >= 0 || Grid.get mine (x+1,y) = Empty)))
          ->
        color.(y*len+x) <- i;
        follow ()
      | Rock -> ()
      | Lift -> color.(y*len+x) <- i
  in
  aux 0 mine.metadata.waterproof_current mine.robot;
  color

let reach_ok mine =
  let color = reachable mine in
  try
    for i=0 to Array.length mine.grid - 1 do
      match mine.grid.(i) with
      | Lambda | Lift -> if color.(i) < 0 then raise Exit
      | _ -> ()
    done;
    true
  with
    Exit -> false

let nexts mine path =
  list_filter_map
    (fun move ->
      try
        let mine' = Moves.apply mine move in
        let mine' = Update.update { mine' with score = pred mine'.score } in
        (* Some (move::path,mine') *)
        let res =
          if
            (try (Hashtbl.find last_time_i_was_there mine'.robot).grid = mine'.grid
             with Not_found -> false)
          then None
          else Some (move::path,mine')
        in
        Hashtbl.replace last_time_i_was_there mine'.robot mine';
        res
      with
      | Won -> raise (FoundWin (move::path, mine.score + mine.collected * 50 - 1))
      | Update.Dead -> None)
    (possible_moves mine)

let rec full depthmax depth acc mine =
  if depth > depthmax then ()
  else begin
    Printf.eprintf "\r[2KDepth: %d/%d\n%s[%dA\r" depth depthmax (Grid.to_color_string mine) (mine.height+1);
    (* Printf.eprintf "[2KDepth: %2d/%2d -- Collected/Score: %d/%d\n" depth depthmax mine.collected mine.score; *)
    (* flush stderr; *)
    let next = nexts mine acc in
    let next =
      rev_concat_map (fun (path,mine) -> nexts mine path) next
    in
    let next =
      rev_concat_map (fun (path,mine) -> nexts mine path) next
    in
    let next =
      rev_concat_map (fun (path,mine) -> nexts mine path) next
    in
    let next =
      List.filter (fun (path,mine) -> reach_ok mine) next
    in
    (* greedy ! *)
    let next = List.sort (fun (_,mine1) (_,mine2) -> mine2.score - mine1.score) next in
    List.iter (fun (path, mine) -> full depthmax (succ depth) path mine) next
  end

let rec breadth depth l =
  Printf.eprintf "[2KDepth: %d\r" depth;
  flush stderr;
  let next_mines =
    rev_concat_map
      (fun (path,mine) -> nexts mine path)
      l
  in
  breadth (succ depth) next_mines

let eval_situation mine =
  let score = ref mine.score in
  let color = reachable mine in
  let canwin = ref true in
  for i=0 to Array.length mine.grid - 1 do
    match mine.grid.(i) with
    | Lambda | Lift when color.(i) >= 0 ->
      score := !score + 25 - color.(i)
    | Lambda ->
      canwin := false;
      score := !score - 25
    | Lift ->
      canwin := false
    | _ -> ()
  done;
  if !canwin then score := !score + 50 * mine.nlambdas
  else score := !score + 25 * mine.nlambdas;
  !score

let rec lookup_path best_so_far step walked_path mine0 =

  Printf.eprintf
    "\r[2K[32mStep/Moves: %d/%d\nScore: %d[0m\n%s\n%s[%dA\r"
    step mine0.moves mine0.score
    (let s = (Moves.rev_path_to_string walked_path) in if String.length s > 80 then "..."^String.sub s (String.length s - 77) 77 else s)
    (Grid.to_color_string mine0) (mine0.height+3);
  flush stderr;

  if let (best,_,_) = !best_so_far in mine0.score > best then
    best_so_far :=
      mine0.score + 25 * mine0.collected, mine0, walked_path;

  (* assert (List.length walked_path = mine.moves); *)
  let rec reachmap walked_path mine =
    let paths = Array.make (Array.length mine.grid) (mine,[]) in
    let len = mine.length in
    let rec aux mine path =
      let (x,y) = mine.robot in
      match paths.(y*len+x) with
      | (mine1, path1) when path1 = [] || mine1.score < mine.score ->
        paths.(y*len+x) <- (mine, path);
        let attempt move =
          if Moves.is_valid mine move then
            try
              let mine = Moves.apply mine move in
              let mine = Update.update mine in
              aux mine (move::path)
            with
            | Won ->
              let end_score = mine.collected * 75 - mine.moves - 1 in
              let (best,_,_) = !best_so_far in
              if end_score > best then
                (let p = move::path@walked_path in
                 Printf.eprintf "[J[31mFound win: %d (%d):[m %s\n"
                   end_score (List.length p) (Moves.rev_path_to_string p);
                 best_so_far := (end_score, mine, p))
            | Update.Dead -> ()
        in
        List.iter attempt all_moves
      | _ -> ()
    in
    aux mine [];
    paths
  in
  let paths = reachmap walked_path mine0 in
  let eval0 = eval_situation mine0 in
  let paths =
    Array.fold_left
      (fun paths (mine,path) ->
        match path with
        | [] -> paths
        | p ->
          (* let reach = reachmap walked_path mine in *)
          (* let eval = *)
          (*   Array.fold_left (\* extract max score *\) *)
          (*     (fun score (mine1,_) -> *)
          (*       if mine1.score > score then mine1.score else score) *)
          (*     mine.score *)
          (*     reach *)
          (* in *)
          let eval = eval_situation mine in
          if eval > eval0 then (eval, mine, p)::paths
          else paths)
      []
    paths
  in
  let paths =
    List.sort (fun (eval1,_,_) (eval2,_,_) -> eval2 - eval1) paths
  in
  List.iter
    (fun (_eval,mine,path) ->
      lookup_path
        best_so_far (succ step) (path@walked_path) mine)
    paths



(* ------------------- *)

let init_mine =
  if Array.length Sys.argv > 1 then
    let f = open_in Sys.argv.(1) in
    let g = Grid.parse f in
    close_in f;
    g
  else
    Grid.parse stdin

let _ =
  prerr_endline "Loaded.";
  let score, mine, path =
    let best_so_far = ref (0, init_mine, []) in
    Sys.catch_break true;
    try
      lookup_path best_so_far 1 [] init_mine;
      (* for depthmax = 1 to 300 do *)
      (*   Printf.eprintf "[2KTrying at depth: %d\r" depthmax; *)
      (*   flush stderr; *)
      (*   Hashtbl.clear last_time_i_was_there; *)
      (*   full depthmax 0 [] init_mine *)
      (* done; *)
      !best_so_far
    with
    | Sys.Break -> !best_so_far
  in
  Printf.eprintf
    "\r[J[31mBest solution found: (%d in %d/%d moves):[0m %s\n"
    score (List.length path) (init_mine.length * init_mine.height)
    (Moves.rev_path_to_string path)
