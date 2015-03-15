(** Partition refinement algorithm. *)

module type S =
sig

type t
(** Type of partition structure *)

type set
(** Type of partitions *)

val create : int -> t
(** Create a partition structure of the given size *)

val length : t -> int
(** Number of partitions *)

val size : set -> t -> int
(** Number of elements of a partition *)

val partition : int -> t -> set
(** [partition i t] returns the index of the partition which contains [i] *)

val iter : set -> (int -> unit) -> t -> unit
(** Iter on elements of a partition. Don't [mark] and [split] in the loop! *)

val fold : set -> (int -> 'a -> 'a) -> t -> 'a -> 'a
(** Fold left to right on elements of a partition. Don't [mark] and [split] in
    the loop! *)

val iter_all : (set -> unit) -> t -> unit
(** Iter on partitions. Don't [mark] and [split] in the loop! *)

val fold_all : (set -> 'a -> 'a) -> t -> 'a -> 'a
(** Fold left to right on partitions. Don't [mark] and [split] in the loop! *)

val mark : int -> t -> unit
(** Mark an element for splitting *)

val split : set -> t -> set
(** Performs splitting and return the set of marked elements *)

val is_marked : set -> t -> bool
(** Returns [true] if some element of the set is marked *)

val is_valid : set -> bool
(** Test whether a splitting succeeded *)

val choose : set -> t -> int
(** Choose any element of a partition *)

val represent : set -> int
(** Associate a unique number to each partition. If the partition is valid, then
    the returned number is guaranteed to be between [0] and [len - 1] when
    [len] is the number of partitions of the structure. *)

end

include S
