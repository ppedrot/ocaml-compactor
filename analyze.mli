type data =
| Int of int
| Ptr of int
| Atm of int (* tag *)

type obj =
| Struct of int * data array (* tag × data *)
| String of string

val parse : in_channel -> (data * obj array)
(** Return the entry point and the reification of the memory out of a marshalled
    structure. *)
