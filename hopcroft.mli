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
    (** The number of states of the automaton. *)
    partitions : state list array;
    (** A set of state partitions initially known to be observationally
        distinct. For instance, if the automaton has the list [l] as accepting
        states, one can set [partitions = [|l|]]. *)
    transitions : transition array;
    (** The transitions of the automaton without duplicates. Assumes that the
        transitions are ordred on their [lbl] component. *)
  }

  val reduce : automaton -> state list array
  (** Associate the array of equivalence classes of the states of an automaton *)

  module SPartition : Partition.S

  val reduce_partition : automaton -> SPartition.t
  (** Same as {!reduce} but more low-level. Instead of constructing the array
      of equivalence classes, it returns their actual partition state. The
      returned partition holds as many objects as the number of states of the
      original automaton, and each class corresponds to the equivalence class
      of the automaton states. *)

end

module Make (Label : OrderedType) : S with type label = Label.t and type state = int
