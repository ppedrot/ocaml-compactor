#include <caml/memory.h>
#include "hopcroft.h"
#include "interface.h"

/*
type transition =
| StringT of string
| TagT of int
| AtomT of int * int (* field number * tag *)
| PFieldT of int (* field number *)
| IFieldT of int * int (* field number * int value *)
*/

void caml_of_transition (value caml_trans, transition* trans) {
  CAMLparam1(caml_trans);
  CAMLlocal1(val);
  switch(Tag_val(caml_trans))
    case 0:
      
    default: return;
}
