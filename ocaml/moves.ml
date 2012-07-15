open Grid.T

module T = struct
  type move = Left | Right | Up | Down | Wait | Abort | Shave

  exception Aborted
  exception Won
end

open T

let char_to_move = function
  | 'L' -> Left
  | 'R' -> Right
  | 'U' -> Up
  | 'D' -> Down
  | 'W' -> Wait
  | 'A' -> Abort
  | 'S' -> Shave
  | c -> raise (Invalid_argument (Printf.sprintf "char_to_move: '%c'" c))

let move_to_char = function
  | Left -> 'L'
  | Right -> 'R'
  | Up -> 'U'
  | Down -> 'D'
  | Wait -> 'W'
  | Abort -> 'A'
  | Shave -> 'S'

let list_iteri f l =
  ignore (
    List.fold_left (fun acc x -> let () = f x acc in succ acc) 0 l
  )

let rev_path_to_string rpath =
  let cs = List.rev_map move_to_char rpath in
  let s = String.make (List.length cs) 'W' in
  list_iteri (fun c i -> s.[i] <- c) cs;
  s

let result (x,y) = function
  | Left -> (x-1, y)
  | Right -> (x+1, y)
  | Up -> (x, y+1)
  | Down -> (x, y-1)
  | _ -> (x, y)

let is_valid mine move =
  match move with
  | Wait | Abort -> true
  | Shave -> mine.metadata.razors > 0
  | Left | Right | Up | Down ->
    try
      let dest = result mine.robot move in
      begin match Grid.get mine dest with
      | Empty | Earth | Lambda | Razor | Trampoline _ -> true
      | Wall -> false
      | Rock | Beard | Target _ | Horock ->
        (move = Left || move = Right) && Grid.get mine (result dest move) = Empty
      | Lift -> mine.collected = mine.nlambdas
      | Robot -> assert false
      end
    with Invalid_argument _ -> false

let apply mine move =
  match move with
  | Wait ->
    { mine with
      moves = succ mine.moves;
      score = pred mine.score;
    }
  | Abort -> raise Aborted
  | Shave ->
    let mine = Grid.copy mine in
    let shave ((x,y) as pos) =
      try
        match Grid.get mine pos with
        | Beard -> mine.grid.(y*mine.length+x) <- Empty
        | _ -> ()
      with Invalid_argument _ -> ()
    in
    let (x,y) = mine.robot in
    List.iter shave
      [(x-1,y-1);(x,y-1);(x+1,y-1);
       (x-1,y);(x+1,y);
       (x-1,y+1);(x,y+1);(x+1,y+1)];
    { mine with
      metadata = { mine.metadata with razors = pred mine.metadata.razors };
      moves = succ mine.moves;
      score = pred mine.score;
    }
  | Left | Right | Up | Down ->
    let dest = result mine.robot move in
    let mine,dest = match Grid.get mine dest with
      | Lambda ->
        { mine with
          collected = succ mine.collected;
          score = mine.score + 25;
        }, dest
      | Razor ->
        { mine with metadata =
            { mine.metadata with razors = succ mine.metadata.razors }
        }, dest
      | Rock | Horock -> Grid.set mine (result dest move) Rock, dest
      | Lift -> raise Won
      | Trampoline _ ->
        let newdest = List.assoc dest mine.metadata.tramap in
        let other_trampolines =
          List.fold_left
            (fun trs (tr,tg) -> if tg = newdest then tr::trs else trs)
            []
            mine.metadata.tramap
        in
        List.fold_left
          (fun mine pos -> Grid.set mine pos Empty)
          mine
          other_trampolines,
        newdest
      | _ -> mine, dest
    in
    let mine = Grid.set mine mine.robot Empty in
    let mine = Grid.set mine dest Robot in
    { mine with
      robot = dest;
      score = pred mine.score;
      moves = succ mine.moves;
    }

let follow mine moves =
  List.fold_left
    (fun mine move ->
      Update.update (apply mine move))
    mine
    moves
