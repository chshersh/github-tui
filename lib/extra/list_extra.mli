(** [in_between ~sep l] creates a new list where [sep] is placed in between
    every element of the list.

    Doesn't insert [sep] when the given list has size 0 or 1. *)
val in_between : sep:'a -> 'a list -> 'a list

(** [max_on to_int l] applies [to_int] to every element of the list and returns
    the maximum value.

    Useful to calculate the longest string a list. *)
val max_on : ('a -> int) -> 'a list -> int

(** [map_and_fold ~f ~init l] folds and maps simultaneously. This function folds
    the list with [f] where [f] takes the current element of the list,
    accumulator and returns a new element of the list of possibly updated
    accumulator. The transormed list and the final value of the accumulator are
    return. *)
val map_and_fold :
  f:('a -> 'acc -> 'b * 'acc) -> init:'acc -> 'a list -> 'b list * 'acc

(** [map_with_fold ~f ~init l] is just like [map_and_fold ~f ~init] except the
    final value of the accumulator is not returned. *)
val map_with_fold :
  f:('a -> 'acc -> 'b * 'acc) -> init:'acc -> 'a list -> 'b list
