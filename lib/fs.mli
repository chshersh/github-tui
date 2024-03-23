(** A definition of a file tree. *)
type tree =
  | File of string
  | Dir of string * tree array

(** Return the name of a given tree node. *)
val file_name : tree -> string

(** Read directory contents recursively from a given file path. *)
val read_tree : string -> tree

(** A cursor for files inside a single directory. *)
type cursor = {
  pos : int;
  files : tree array;
}

(** Return the currently selected file in file cursor. *)
val file_at : cursor -> tree option

(** A file tree zipper that allows to traverse the tree in four directions. *)
type zipper = {
  parents : cursor list;
  current : cursor;
}

(** Constructs a zipper from the contents of a given directory. *)
val zip_it : tree array -> zipper

(** Returns the list of parents names in reverse order. *)
val zipper_parents : zipper -> string list

(** Move to the next file within the same directory. Cycles. *)
val go_down : zipper -> zipper

(** Move to the previous file within the same directory. Cycles. *)
val go_up : zipper -> zipper

(** Move to the directory under the current cursor. Doesn't move inside files or empty directories. *)
val go_next : zipper -> zipper

(** Move to the parent directory. *)
val go_back : zipper -> zipper
