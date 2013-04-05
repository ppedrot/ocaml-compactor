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

typedef enum {
  VAL_INT, // unboxed integer
  VAL_PTR, // internal pointer
  VAL_ABS, // abstract pointer
} value_tag;

union value_field {
  int_value int_val;
  ptr_value ptr_val;
  abs_value abs_val;
};

typedef struct value_data {
  value_tag value_data_tag;
  union value_field value_data_val;
} value_data;

typedef struct tbl_value {
  size_t tbl_value_len;
  value_data* tbl_value_tbl;
} tbl_value;
/* OCaml structured values */

#endif
