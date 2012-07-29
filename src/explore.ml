open Moves.T
open Grid.T

let all_moves = [Left; Right; Up; Down; Shave](* ; Wait] *)

exception FoundWin of move list * int

let last_time_i_was_there = (Hashtbl.create 87 : (pos, mine) Hashtbl.t)

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
        color.(y*len+x) <- i+1;
        follow x y
      | Rock | Horock -> ()
      | Lift -> color.(y*len+x) <- i
  in
  aux 0 mine.metadata.razors mine.metadata.waterproof_current mine.robot;
  color

let eval_situation mine =
  let score = ref mine.score in
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
  let disp = (!maxx - !minx) + (!maxy - !miny) in
  score := !score - disp;
  if !canwin then score := !score + 50 * mine.nlambdas
  else score := !score - 100 * mine.nlambdas;
  score := !score + mine.metadata.waterproof_current * 2;
  score := !score + mine.metadata.razors * 2;
  !score

let debug =
  try
    match Sys.getenv "ICFP_DEBUG" with
    | "" | "0" -> false
    | _ -> true
  with Not_found -> false

let rec lookup_path best_so_far allow_worse step walked_path mine0 =

  let eval0 = eval_situation mine0 in

  if debug then begin
    Printf.eprintf
      "\r[2K[32mStep: %d Moves: %d/%d\nScore: %d [32mEval: %d[0m\n%s\n%s[%dA\r"
      step mine0.moves (mine0.length * mine0.height) mine0.score eval0
      (let s = (Moves.rev_path_to_string walked_path) in if String.length s > 80 then "..."^String.sub s (String.length s - 77) 77 else s)
      (Grid.to_color_string mine0) (mine0.height+3);
    flush stderr
  end;

  if let (best,_,_) = !best_so_far in mine0.score > best then
    best_so_far :=
      mine0.score + 25 * mine0.collected, mine0, walked_path;

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
  let paths_array = reachmap walked_path mine0 eval0 in
  let paths =
    Array.fold_left
      (fun paths (mine,path,eval) ->
        match path with
        | [] -> paths
        | path when eval >= eval0 -> (mine, path, eval)::paths
        | _ -> paths)
      []
    paths_array
  in
  let paths =
    List.sort (fun (_,_,eval1) (_,_,eval2) -> eval2 - eval1) paths
  in
  List.iter
    (fun (mine,path,_eval) ->
      lookup_path
        best_so_far allow_worse (succ step) (path@walked_path) mine)
    paths;
  if allow_worse > 0 then
    let paths =
      Array.fold_left
        (fun paths (mine,path,eval) ->
          match path with
          | [] -> paths
          | path when eval < eval0 -> (mine, path, eval)::paths
          | _ -> paths)
        []
        paths_array
    in
    let paths =
      List.sort (fun (_,_,eval1) (_,_,eval2) -> eval2 - eval1) paths
    in
    List.iter
      (fun (mine,path,_eval) ->
        lookup_path
          best_so_far (pred allow_worse) (succ step) (path@walked_path) mine)
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
      let rec attempt allow_worse =
        lookup_path best_so_far allow_worse 1 [] init_mine;
        if allow_worse <= 12 then
          (prerr_endline "\r[J[31mBroadening search[m";
           attempt (succ allow_worse))
      in
      attempt 0;
      !best_so_far
    with
    | Sys.Break -> !best_so_far
  in
  Printf.eprintf
    "\r[J[31mBest solution found: %d in %d/%d moves:[0m\n"
    score (List.length path) (init_mine.length * init_mine.height);
  flush stderr;
  print_endline (Moves.rev_path_to_string path)
