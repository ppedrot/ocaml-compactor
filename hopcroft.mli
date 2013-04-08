(** Hopcroft algorithm *)

module type OrderedType =
sig
  type t
  val compare : t -> t -> int
end

module type S =
sig
  type label
  type state
  type transition = {
    lbl : label;
    src : state;
    dst : state;
  }
  type automaton = {
    states : int;
    (** The number of states of the automaton *)
    final_states : state array;
    (** The final states *)
    transitions : transition array;
    (** The transitions of the automaton without duplicates *)
  }
  val reduce : automaton -> state list array
  (** Associate the array of equivalence classes of the states of an automaton *)
end

module Make (Label : OrderedType) : S with type label = Label.t and type state = int
