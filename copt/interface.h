#include <caml/intext.h>
#include <caml/memory.h>

#ifndef CAML_COMPACTOR_INTERFACE_H
#define CAML_COMPACTOR_INTERFACE_H

/* Scalars */

typedef long int_value;
/* OCaml int */

typedef size_t ptr_value;
/* Absolute pointer in the memory reification */

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

/* Structured data */

typedef enum {
  STRUCT_TBL, // Array
  STRUCT_STR, // String
} struct_tag;

typedef struct str_struct {
  size_t str_value_len;
  char* str_value_val;
} str_struct;
/* OCaml string */

typedef struct tbl_struct {
  header_t tbl_value_hdr;
  size_t tbl_value_len;
  value_data* tbl_value_tbl;
} tbl_struct;
/* OCaml structured values */

union struct_field {
  str_struct str_val;
  tbl_struct tbl_val;
};

typedef struct struct_data {
  struct_tag struct_data_tag;
  union struct_field struct_data_val;
} struct_data;

#endif
