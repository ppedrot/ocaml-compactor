type data =
| Int of int
| Ptr of int
| Atm of int (* tag *)
| Fun of int (* address *)

type obj =
| Struct of int * data array (* tag × data *)
| String of string

type event =
| RInt of int
| RBlock of int * int (** tag × len *)
| RString of string
| RPointer of int
  (** Pointer to the object located [n] times before the current position *)
| RCode of int (** Address of a function *)

type header = {
  magic : string;
  (** Magic number of the marshalled structure *)
  length : int;
  (** Length of the data in bytes *)
  size32 : int;
  (** Memory taken by the unmarshalled data on 32-bit platforms in words *)
  size64 : int;
  (** Memory taken by the unmarshalled data on 64-bit platforms in words *)
  objects : int
  (** Number of objects in memory *)
}

type 'a listener = {
  header : header -> 'a;
  event : event -> 'a -> 'a;
  close : 'a -> 'a;
}

val parse_channel : in_channel -> (data * obj array)
val parse_string : string -> (data * obj array)

val listen_channel : in_channel -> 'a listener -> 'a
val listen_string : string -> 'a listener -> 'a

(** {6 Functorized version} *)

module type Input =
sig
  type t
  val input_byte : t -> int
  (** Input a single byte *)
  val input_binary_int : t -> int
  (** Input a big-endian 31-bits signed integer *)
end
(** Type of inputs *)

module type S =
sig
  type input
  val parse : input -> (data * obj array)
  (** Return the entry point and the reification of the memory out of a
      marshalled structure. *)
  val listen : input -> 'a listener -> 'a
end

module Make (M : Input) : S with type input = M.t
(** Functorized version of the previous code. *)
