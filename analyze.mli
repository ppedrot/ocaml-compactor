type data =
| Int of int
| Ptr of int
| Atom of int (* tag *)

type obj =
| Struct of int * data array (* tag × data *)
| String of string

val parse : in_channel -> obj array

