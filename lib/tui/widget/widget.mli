module Issue = Issue
module Pr = Pr
module Generic = Generic

(** Print the about info into a pretty doc **)
val about_doc : Model.t -> Pretty.Doc.t

(** Draw the code tab *)
val code_tab : is_selected:bool -> Pretty.Doc.t

(** Draw the issues tab *)
val issues_tab : is_selected:bool -> Pretty.Doc.t

(** Draw the pull requests tab *)
val pull_requests_tab : is_selected:bool -> Pretty.Doc.t

(** Draw the working directory **)
val pwd : string -> Fs.zipper -> Pretty.Doc.t

(** Draw the file view **)
val file_view : Fs.zipper -> Pretty.Doc.t
