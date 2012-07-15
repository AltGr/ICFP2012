module T: sig
  type pos = int * int

  type square =
  | Robot | Wall | Rock | Lambda | Lift | Earth | Empty
  | Trampoline of char | Target of char
  | Beard | Razor
  | Horock

  type metadata = {
    flooding: int;
    water: int;
    waterproof: int;
    waterproof_current: int;
    growth: int;
    razors: int;
    tramap: (pos * pos) list;
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

open T

val copy: mine -> mine

val parse: in_channel -> mine

val check: mine -> bool

val get: mine -> pos -> square

val set: mine -> pos -> square -> mine

val iteri: (pos -> square -> unit) -> mine -> unit

val mapi: (pos -> square -> square) -> mine -> mine

val fold: ('acc -> square -> 'acc) -> 'acc -> mine -> 'acc

val foldi: (pos -> 'acc -> square -> 'acc) -> 'acc -> mine -> 'acc

val to_string: mine -> string

val to_color_string: mine -> string

