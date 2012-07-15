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
  let color = Array.make (Array.length mine.grid) false in
  let len = mine.length in
  let rec aux ((x,y) as pos) =
    if not color.(y*len+x) then
      let follow () =
        if x > 0 then aux (x-1,y);
        if x < mine.length - 1 then aux (x+1,y);
        if y > 0 then aux (x,y-1);
        if y < mine.length - 1 then aux (x,y+1)
      in
      match Grid.get mine pos with
      | Wall -> ()
      | Lambda | Earth | Robot | Empty ->
        color.(y*len+x) <- true;
        follow ()
      | Rock when (* Rock can move (approximatively) *)
          (y > 0 (* Rock can fall *)
           && (color.((y-1)*len+x)
               || match Grid.get mine (x,y-1) with
                 | Empty -> true
                 | Rock when
                     (x > 0
                      && (color.((y-1)*len+x-1) || Grid.get mine (x-1,y-1) = Empty))
                     || (x < len-1
                         && (color.((y-1)*len+x+1) || Grid.get mine (x+1,y-1) = Empty))
                     -> true
                 | Lambda when
                     x < len-1
                     && (color.((y-1)*len+x+1) || Grid.get mine (x+1,y-1) = Empty)
                     -> true
                 | _ -> false))
          || (x>0 && x<len-1 (* Rock can be pushed *)
              && (
                (color.(y*len+x-1) || Grid.get mine (x-1,y) = Empty)
                && (color.(y*len+x+1) || Grid.get mine (x+1,y) = Empty)))
          ->
        color.(y*len+x) <- true;
        follow ()
      | Rock -> ()
      | Lift -> color.(y*len+x) <- true
  in
  aux mine.robot;
  color

let reach_ok mine =
  let color = reachable mine in
  try
    for i=0 to Array.length mine.grid - 1 do
      match mine.grid.(i) with
      | Lambda | Lift -> if not color.(i) then raise Exit
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


let rec reachmap step walked_path mine =
  Printf.eprintf
    "\r[2K[32mStep/Moves: %d/%d\nScore: %d[0m\n%s[%dA\r"
    step mine.moves mine.score
    (Grid.to_color_string mine) (mine.height+2);
  flush stderr;
  (* assert (List.length walked_path = mine.moves); *)
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
            if reach_ok mine then
              aux mine (move::path)
          with
          | Won ->
            raise (FoundWin (move::path@walked_path,
                             mine.collected * 75 - mine.moves - 1))
          | Update.Dead -> ()
      in
      List.iter attempt all_moves
    | _ -> ()
  in
  aux mine [];
  (* for y = mine.height - 1 downto 0 do *)
  (*   for x = 0 to mine.length -1 do *)
  (*     let (p,s) = paths.(y*mine.length+x) in *)
  (*     if s = min_int then Printf.printf " (------)" *)
  (*     else Printf.printf " (%2d,%3d)" (List.length p + List.length walked_path) s *)
  (*   done; *)
  (*   print_newline (); *)
  (* done; *)
  let paths =
    Array.fold_left
      (fun acc ((mine1,_) as p) -> if mine1.score > mine.score then p::acc else acc)
      []
      paths
  in
  let paths = List.sort (fun (mine1,_) (mine2,_) -> mine2.score - mine1.score) paths in
  List.iter
    (fun (mine,path) ->
      reachmap (succ step) (path@walked_path) mine)
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

let list_iteri f l =
  ignore (
    List.fold_left (fun acc x -> let () = f x acc in succ acc) 0 l
  )

let _ =
  prerr_endline "Loaded.";
  let winwinwin, score =
    try
      reachmap 1 [] init_mine;
      (* for depthmax = 1 to 300 do *)
      (*   Printf.eprintf "[2KTrying at depth: %d\r" depthmax; *)
      (*   flush stderr; *)
      (*   Hashtbl.clear last_time_i_was_there; *)
      (*   full depthmax 0 [] init_mine *)
      (* done; *)
      raise Not_found
    with FoundWin (w,score) -> w, score
  in
  let moves = List.rev_map Moves.move_to_char winwinwin in
  let s = String.make (List.length moves) 'W' in
  list_iteri (fun c i -> s.[i] <- c) moves;
  Printf.eprintf "[31mFound win (%d in %d moves):[0m %s\n" score (String.length s) s

