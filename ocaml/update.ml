open Grid.T

let update_square mine mine' (x,y) square =
  let get = Grid.get mine
  and set (x,y) square = mine'.grid.(y*mine.length+x) <- square
  in
  match square with
  | (Rock | Horock) as r ->
    let fall ((x',y') as pos') =
      set (x,y) Empty;
      if r = Horock
      && (try Grid.get mine (x',y'-1) <> Empty with Invalid_argument _ -> false)
      then
        set pos' Lambda
      else
        set pos' r
    in
    begin match get (x,y-1) with
    | Empty -> fall (x,y-1)
    | Rock ->
      if get (x+1,y) = Empty && get (x+1,y-1) = Empty then
        fall (x+1,y-1)
      else if get (x-1,y) = Empty && get (x-1,y-1) = Empty then
        fall (x-1,y-1)
    | Lambda ->
      if get (x+1,y) = Empty && get (x+1,y-1) = Empty then
        fall (x+1,y-1)
    | _ -> ()
    end
  | Beard when mine.moves mod mine.metadata.growth = 0 ->
    let grow pos =
      try
        match Grid.get mine pos with
        | Empty -> set pos Beard
        | _ -> ()
      with Invalid_argument _ -> ()
    in
    List.iter grow
      [(x-1,y-1);(x,y-1);(x+1,y-1);
       (x-1,y);(x+1,y);
       (x-1,y+1);(x,y+1);(x+1,y+1)]
  | _ -> ()

let isdead mine0 mine =
  let (x,y) = mine.robot in
  try
    if mine.metadata.waterproof_current < 0 then true
    else if mine.moves > mine.length * mine.height then true
    else if Grid.get mine0 (x,y+1) = Empty then
      match Grid.get mine (x,y+1) with Rock | Horock | Lambda -> true | _ -> false
    else false
  with Invalid_argument _ -> false

exception Dead

let update mine0 =
  let mine = Grid.copy mine0 in
  Grid.iteri (update_square mine0 mine) mine0;
  let (x,y) = mine.robot in
  let metadata = mine.metadata in
  let metadata = {
    metadata with waterproof_current =
      if y < metadata.water
      then pred metadata.waterproof_current
      else metadata.waterproof
  } in
  let metadata = {
    metadata with water =
      if metadata.flooding > 0 && mine.moves mod metadata.flooding = 0
      then succ metadata.water
      else metadata.water
  } in
  let mine = { mine with metadata } in
  if isdead mine0 mine then raise Dead;
  mine
