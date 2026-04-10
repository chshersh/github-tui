(** Module to manage gitignored files visability **)
module Gitignore = Gitignore

(** A parsed .gitignore rule *)
type rule = {
  pattern : string;
  negated : bool;
}

(** Parse .gitignore content into a list of rules. Supports ! negation *)
val parse : string -> rule list

(** Check if a path is ignored according to the parsed rules *)
val is_ignored : string -> rule list -> bool

(** Match a path against a single rule *)
val matches : string -> rule -> bool

val find_gitignore : string -> string option
