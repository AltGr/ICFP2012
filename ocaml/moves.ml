open Grid.T

module T = struct
  type move = Left | Right | Up | Down | Wait | Abort

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
  | c -> raise (Invalid_argument (Printf.sprintf "char_to_move: '%c'" c))

let move_to_char = function
  | Left -> 'L'
  | Right -> 'R'
  | Up -> 'U'
  | Down -> 'D'
  | Wait -> 'W'
  | Abort -> 'A'

let result (x,y) = function
  | Left -> (x-1, y)
  | Right -> (x+1, y)
  | Up -> (x, y+1)
  | Down -> (x, y-1)
  | _ -> (x, y)

let is_valid mine move =
  match move with
  | Wait | Abort -> true
  | Left | Right | Up | Down ->
    try
      let dest = result mine.robot move in
      begin match Grid.get mine dest with
      | Empty | Earth | Lambda -> true
      | Wall -> false
      | Rock -> (move = Left || move = Right) && Grid.get mine (result dest move) = Empty
      | Lift -> mine.nlambdas = 0
      | Robot -> assert false
      end
    with Invalid_argument _ -> false

let apply mine move =
  match move with
  | Wait -> { mine with moves = succ mine.moves }
  | Abort -> raise Aborted
  | Left | Right | Up | Down ->
    let dest = result mine.robot move in
    let mine = match Grid.get mine dest with
      | Lambda ->
        { mine with
          nlambdas = pred mine.nlambdas;
          collected = succ mine.collected;
          score = mine.score + 25;
        }
      | Rock -> Grid.set mine (result dest move) Rock
      | Lift -> raise Won
      | _ -> mine
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
      let mine = apply mine move in
      Update.update (apply mine move))
    mine
    moves
