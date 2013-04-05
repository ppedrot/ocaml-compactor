#include <caml/intext.h>
#include <caml/memory.h>

#ifndef CAML_COMPACTOR_INTERFACE_H
#define CAML_COMPACTOR_INTERFACE_H

typedef long int_value;
/* OCaml int */

typedef size_t ptr_value;
/* Absolute pointer in the memory reification */

typedef struct str_value {
  size_t str_value_len;
  char* str_value_val;
} str_value;
/* OCaml string */

typedef value* abs_value;
/* Abstract value in OCaml heap */

#endif
