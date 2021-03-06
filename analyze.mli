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
  iheader : header -> 'a;
  ievent : event -> 'a -> 'a;
  iclose : 'a -> 'a;
}

type echoer = {
  oevent : event -> unit;
  oclose : unit -> header;
}

val parse_channel : in_channel -> (data * obj array)
val parse_string : string -> (data * obj array)

val listen_channel : in_channel -> 'a listener -> 'a
val listen_string : string -> 'a listener -> 'a

val echo_channel : out_channel -> echoer

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

module type IS =
sig
  type input
  val parse : input -> (data * obj array)
  (** Return the entry point and the reification of the memory out of a
      marshalled structure. *)
  val listen : input -> 'a listener -> 'a
end

module IMake (M : Input) : IS with type input = M.t
(** Functorized version of the previous code. *)

module type Output =
sig
  type t
  val output_byte : t -> int -> unit
  (** Output a single byte *)
  val output_binary_int : t -> int -> unit
  (** Output a big-endian 31-bits signed integer *)
  val pos : t -> int
  (** Get the current position in the output stream *)
  val seek : t -> int -> unit
  (** Go to the given position in the output stream *)
end

module type OS =
sig
  type output
  val echo : output -> echoer
end

module OMake (M : Output) : OS with type output = M.t
(** Functorized version of the previous code. *)
