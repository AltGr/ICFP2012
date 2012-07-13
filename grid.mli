type pos = int * int

type square = Robot | Wall | Rock | Lambda | Lift | Earth | Empty

type mine = {
  grid: square array;
  length: int;
  height: int;
  robot: pos;
  lift: pos;
  nlambdas: int;
}

val parse: unit -> mine

val check: mine -> bool

val get: mine -> pos -> square

val set: mine -> pos -> square -> mine

val fold: ('acc -> square -> 'acc) -> 'acc -> mine -> 'acc

val foldi: (pos -> 'acc -> square -> 'acc) -> 'acc -> mine -> 'acc

val to_string: mine -> string

