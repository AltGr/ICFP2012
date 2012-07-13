open Grid.T

let update_square mine mine' (x,y) square =
  let get = Grid.get mine
  and set (x,y) square = mine'.grid.(y*mine.length+x) <- square
  in
  match square with
  | Rock ->
    begin match get (x,y-1) with
    | Empty -> set (x,y) Empty; set (x,y-1) Rock
    | Rock ->
      if get (x+1,y) = Empty && get (x+1,y-1) = Empty then
        (set (x,y) Empty; set (x+1,y-1) Rock)
      else if get (x-1,y) = Empty && get (x-1,y-1) = Empty then
        (set (x,y) Empty; set (x-1,y-1) Rock)
    | Lambda ->
      if get (x+1,y) = Empty && get (x+1,y-1) = Empty then
        (set (x,y) Empty; set (x+1,y-1) Rock)
    | _ -> ()
    end
  | _ -> ()

let update mine =
  let mine' = Grid.copy mine in
  Grid.iteri (update_square mine mine') mine;
  mine'
