module T = struct
  type pos = int * int

  type square = Robot | Wall | Rock | Lambda | Lift | Earth | Empty

  type mine = {
    grid: square array;
    length: int;
    height: int;
    robot: pos;
    lift: pos;
    nlambdas: int;
    collected: int;
    score: int;
  }
end

include T

let copy mine = { mine with grid = Array.copy mine.grid }

let char_to_square = function
  | 'R' -> Robot
  | '#' -> Wall
  | '*' -> Rock
  | '\\' -> Lambda
  | 'L' -> Lift
  | '.' -> Earth
  | ' ' -> Empty
  | c -> failwith (Printf.sprintf "Unknown square character '%C'" c)

let square_to_char = function
  | Robot -> 'R'
  | Wall -> '#'
  | Rock -> '*'
  | Lambda -> '\\'
  | Lift -> 'L'
  | Earth -> '.'
  | Empty -> ' '

let check _ = assert false

let array_find_one f a =
  match
    Array.fold_left
      (fun acc x -> match acc with
       | (false,i) ->
         (if f x then (true, i) else (false, i+1))
       | r -> r)
      (false, 0)
      a
  with
  | (true, i) -> i
  | _ -> raise Not_found

let parse chan =
  let length_ref = ref 0 in
  let rec aux acc =
    let optline = try Some (input_line chan) with End_of_file -> None in
    match optline with
    | Some line when String.length line > 0 ->
      let len = String.length line in
      length_ref := max !length_ref len;
      let arr =
        Array.init len (fun i -> char_to_square line.[i])
      in
      aux (arr::acc)
    | _ -> acc
  in
  let lines = aux [] in
  let length = !length_ref in
  let lines = (* add padding *)
    List.map
      (fun a -> let a' = Array.make length Empty in Array.blit a 0 a' 0 (Array.length a); a')
      lines
  in
  let grid = Array.concat lines in
  let height = Array.length grid / length in
  let robot =
    let abs = array_find_one (function Robot -> true | _ -> false) grid in
    abs mod length, abs / length
  in
  let lift =
    let abs = array_find_one (function Lift -> true | _ -> false) grid in
    abs mod length, abs / length
  in
  let nlambdas =
    Array.fold_left
      (fun acc -> function Lambda -> succ acc | _ -> acc) 0 grid
  in
  { grid; length; height; robot; lift; nlambdas; collected = 0; score = 0 }

let get mine (x,y) =
  if x < 0 || mine.length <= x || y < 0 || mine.height <= y
  then raise (Invalid_argument "Grid.get: bad coords");
  mine.grid.(y * mine.length + x)

let set mine (x,y) square =
  if x < 0 || mine.length <= x || y < 0 || mine.height <= y
  then raise (Invalid_argument "Grid.set: bad coords");
  let mine' = copy mine in
  mine'.grid.(y * mine.length + x) <- square;
  mine'

let iteri f mine = Array.iteri (fun i -> f (i mod mine.length, i / mine.length)) mine.grid

let mapi f mine = assert false (* Array.mapi f mine.grid *)

let fold f acc mine = Array.fold_left f acc mine.grid

let foldi f acc mine =
  let (i,acc) =
    Array.fold_left
      (fun (i,acc) x ->
        let pos = i mod mine.length, i / mine.length in
        succ i, f pos acc x)
      (0,acc) mine.grid
  in acc

let string_init len f =
  let s = String.make len ' ' in
  for i=0 to len-1 do String.unsafe_set s i (f i) done;
  s

let to_string mine =
  string_init ((mine.length + 1) * mine.height)
    (fun i ->
      let x = i mod (mine.length + 1) in
      let y = mine.height - i / (mine.length + 1) - 1 in
      if x = mine.length then '\n'
      else square_to_char (get mine (x,y)))
