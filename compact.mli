open Analyze

val reduce : data -> obj array -> data * obj array
(** Compute the maximal sharing of a memory *)

val represent : data -> obj array -> Obj.t
(** Translate the reified data structure into an OCaml object. *)
