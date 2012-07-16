open Moves.T
open Grid.T

let all_moves = [Left; Right; Up; Down; Shave](* ; Wait] *)

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
  let rec aux i razors waterproof ((x,y) as pos) =
    if i > max_moves || waterproof < 0 then ()
    else if color.(y*len+x) < 0 then
      let waterproof =
        if y < mine.metadata.water then waterproof - 1 else mine.metadata.waterproof
      in
      let follow x y =
        let razors =
          match Grid.get mine pos with
          | Beard -> pred razors
          | Razor -> succ razors
          | _ -> razors
        in
        let aux' = aux (succ i) razors waterproof in
        if x > 0 then aux' (x-1,y);
        if x < mine.length - 1 then aux' (x+1,y);
        if y > 0 then aux' (x,y-1);
        if y < mine.length - 1 then aux' (x,y+1)
      in
      match Grid.get mine pos with
      | Wall | Target _ -> ()
      | Beard when razors > 0->
        color.(y*len+x) <- i;
        follow x y
      | Beard -> ()
      | Lambda | Earth | Robot | Empty | Razor ->
        color.(y*len+x) <- i;
        follow x y
      | Trampoline _ ->
        color.(y*len+x) <- i;
        let (targ_x,targ_y) = List.assoc (x,y) mine.metadata.tramap in
        color.(targ_y*len+targ_x) <- i;
        follow targ_x targ_y
      | Rock | Horock when (* Rock can move (approximatively) *)
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
        follow x y
      | Rock | Horock -> ()
      | Lift -> color.(y*len+x) <- i
  in
  aux 0 mine.metadata.razors mine.metadata.waterproof_current mine.robot;
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
  let score = ref (mine.collected * 50) in
  let color = reachable mine in
  let canwin = ref true in
  let (minx,miny,maxx,maxy) = (ref mine.length, ref mine.height, ref 0, ref 0) in
  for i=0 to Array.length mine.grid - 1 do
    let c = color.(i) in
    match mine.grid.(i) with
    | (Lambda | Horock) when c < 0 ->
      canwin := false;
      score := !score - 50
    | Lift when c < 0 ->
      canwin := false
    | (Lambda | Lift | Horock) as sq ->
      let (x,y) = i mod mine.length, i / mine.length in
      if x < !minx then minx := x else if x > !maxx then maxx := x;
      if y < !miny then miny := y else if y > !maxy then maxy := y;
      if sq = Horock then score := !score - 75
    | _ -> ()
  done;
  (* let disp = (!maxx - !minx) + (!maxy - !miny) in *)
  (* score := !score - disp * disp; *)
  if !canwin then score := !score + 25 * mine.nlambdas
  else score := !score - 25 * mine.nlambdas;
  score := !score + mine.metadata.waterproof_current * 2;
  score := !score + mine.metadata.razors * 2;
  !score

let rec lookup_path best_so_far toptake eval_cut step walked_path mine0 =

  let eval0 = eval_situation mine0 in

  Printf.eprintf
    "\r[2K[32mStep: %d Moves: %d/%d\nScore: %d [32mEval: %d[0m\n%s\n%s[%dA\r"
    step mine0.moves (mine0.length * mine0.height) mine0.score eval0
    (let s = (Moves.rev_path_to_string walked_path) in if String.length s > 80 then "..."^String.sub s (String.length s - 77) 77 else s)
    (Grid.to_color_string mine0) (mine0.height+3);
  flush stderr;

  if let (best,_,_) = !best_so_far in mine0.score > best then
    best_so_far :=
      mine0.score + 25 * mine0.collected, mine0, walked_path;

  (* assert (List.length walked_path = mine.moves); *)
  let reachmap walked_path mine eval =
    let paths = Array.make (Array.length mine.grid) (mine,[],min_int) in
    let len = mine.length in
    let rec aux mine path eval =
      let (x,y) = mine.robot in
      match paths.(y*len+x) with
      | (mine1, path1, eval1) when eval1 < eval ->
        paths.(y*len+x) <- (mine, path, eval);
        let attempt move =
          if Moves.is_valid mine move then
            try
              let mine = Moves.apply mine move in
              let mine = Update.update mine in
              aux mine (move::path) (eval_situation mine)
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
    aux mine [] eval;
    paths
  in
  let paths = reachmap walked_path mine0 eval0 in
  let paths =
    Array.fold_left
      (fun paths (mine,path,eval) ->
        match path with
        | [] -> paths
        | path ->
          (* let reach = reachmap walked_path mine in *)
          (* let eval = *)
          (*   Array.fold_left (\* extract max score *\) *)
          (*     (fun score (mine1,_) -> *)
          (*       if mine1.score > score then mine1.score else score) *)
          (*     mine.score *)
          (*     reach *)
          (* in *)
          if eval >= eval0 then (mine, path, eval)::paths
          else paths)
      []
    paths
  in
  let paths =
    List.sort (fun (_,_,eval1) (_,_,eval2) -> eval2 - eval1) paths
  in
  (* List.iter (fun (_,_,a) -> Printf.eprintf " %3d" a) paths; *)
  (* prerr_newline(); *)
  let rec descend ntake paths =
    if ntake >= 0 then
      match paths with
      | (mine,path,_eval)::r ->
        lookup_path
          best_so_far toptake eval_cut (succ step) (path@walked_path) mine;
        descend (pred ntake) r
      | [] -> ()
  in
  descend toptake paths



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
      let rec retry toptake eval_cut =
        if toptake > 0 then (
          lookup_path best_so_far toptake eval_cut 1 [] init_mine;
          prerr_endline "\r[J[31mBroadening search[m";
          retry (toptake * 2) (eval_cut - 20)
        )
      in
      retry 10 80;
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
    (Moves.rev_path_to_string path);
  print_endline (Moves.rev_path_to_string path)
