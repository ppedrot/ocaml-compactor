open Analyze

module IntSet : Set.S with type elt = int

val back_closure : obj array -> IntSet.t -> IntSet.t
(** Backward transitive closure of the memory *)

val forth_closure : obj array -> IntSet.t -> IntSet.t
(** Forward transitive closure of the memory *)

val pr_dot_mem : out_channel -> (data * obj array) -> unit
(** Print memory as a dot graph *)
