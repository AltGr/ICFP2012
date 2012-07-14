module T = struct
  type pos = int * int

  type square = Robot | Wall | Rock | Lambda | Lift | Earth | Empty

  type metadata = {
    flooding: int;
    water: int;
    waterproof: int;
    waterproof_current: int;
  }

  type mine = {
    grid: square array;
    length: int;
    height: int;
    robot: pos;
    lift: pos;
    nlambdas: int;
    collected: int;
    moves: int;
    score: int;
    metadata: metadata;
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

let string_slice cut str =
  let rec aux pos =
    try
      let i = String.index_from str pos cut in
      if i==pos then aux (succ pos)
      else String.sub str pos (i - pos) :: aux (succ i)
    with Not_found | Invalid_argument _ ->
      let l = String.length str in
      if l==pos then []
      else [ String.sub str pos (l - pos) ]
  in
  aux 0

let parse chan =
  let length_ref = ref 0 in
  let rec aux acc = (* parse the map *)
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
  let rec parse_metadata meta =
    try
      let li = input_line chan in
      let meta =
        match string_slice ' ' li with
        | ["Water"; n] -> { meta with water = int_of_string n }
        | ["Flooding"; n] -> { meta with flooding = int_of_string n }
        | ["Waterproof"; n] ->
          let wp = int_of_string n in
          { meta with waterproof = wp; waterproof_current = wp }
        | _ -> prerr_endline ("Warning: unrecognised input \""^li^"\""); meta
      in
      parse_metadata meta
    with End_of_file -> meta
  in
  let metadata =
    parse_metadata {
      flooding = 0;
      water = 0;
      waterproof = 10;
      waterproof_current = 10;
    }
  in
  {
    grid; length; height; robot; lift; nlambdas;
    collected = 0; score = 0; moves = 0;
    metadata;
  }

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
  string_init ((mine.length + 2) * mine.height)
    (fun i ->
      let x = i mod (mine.length + 2) - 1 in
      let y = mine.height - i / (mine.length + 2) - 1 in
      if x < 0 then
        if y < mine.metadata.water then '~'
        else ' '
      else if x = mine.length then '\n'
      else square_to_char (get mine (x,y)))
  (* ^ Printf.sprintf *)
  (*   "\nWater %d\nWaterproof %d\n" *)
  (*   mine.metadata.water *)
  (*   mine.metadata.waterproof_current *)

let to_color_string mine =
  let s = to_string mine in
  let charlen = 6 in
  let s' = String.make (String.length s * charlen) ' ' in
  for i=0 to String.length s - 1 do
    let colorcode = match s.[i] with
      | '#' -> 38
      | '\\' -> 36
      | '.' -> 33
      | '*' -> 34
      | 'R' -> 31
      | 'L' -> 32
      | '~' -> 44
      | '\n' -> 0
      | _ -> 39
    in
    let ns = Printf.sprintf "[%02dm%c" colorcode s.[i] in
    String.blit ns 0 s' (i*charlen) charlen
  done;
  s'
