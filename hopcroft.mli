(** Hopcroft algorithm *)

type ('label, 'state) transition = {
  lbl : 'label;
  src : 'state;
  dst : 'state;
}

type ('label, 'state) automaton = {
  states : int;
  (** The number of states of the automaton *)
  final_states : 'state array;
  (** The final states *)
  transitions : ('label, 'state) transition array;
  (** The transitions of the automaton without duplicates *)
}

module type OrderedType =
sig
  type t
  val compare : t -> t -> int
end

module type S =
sig
  type label
  type state = int
  val reduce : (label, state) automaton -> state list array
  (** Associate the array of equivalence classes of the states of an automaton *)
end

module Make (Label : OrderedType) : S with type label = Label.t
