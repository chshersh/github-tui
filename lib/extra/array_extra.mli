(** [of_sub_array ~offset ~len arr] returns a list of elements from [arr]
    starting from [offset] with length no more than [len]. *)
val of_sub_array : offset:int -> len:int -> 'a array -> 'a list

(** [max_on to_int arr] applies [to_int] to every element of the array and
    returns the maximum value.

    Useful to calculate the longest string an array. *)
val max_on : ('a -> int) -> 'a array -> int
