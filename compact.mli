open Analyze

val reduce : data -> obj array -> data * obj array
(** Compute the maximal sharing of a memory *)

val represent : data -> obj array -> Obj.t
(** Translate the reified data structure into an OCaml object. *)

val share : data -> obj array -> Obj.t
(** Equivalent to the composition of {!reduce} and {!represent}, but more
    efficient. *)

val transduce : in_channel -> out_channel -> unit
(** Read a marshalled data structure on the input channel, reduce it and
    outputs the result on the output channel. Both channels must be seekable. *)
