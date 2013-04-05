#include "partition.h"

typedef struct intfield {
  int intfield_num;
  int intfield_val;
} intfield;

typedef struct string {
  int string_len;
  char* string_ptr;
} string;

typedef union label {
  string lbl_str;
  int lbl_tag;
  int lbl_ptr;
  intfield lbl_atm;
  intfield lbl_int;
} label;

typedef enum {
  LBLSTR,
  LBLTAG,
  LBLATM,
  LBLPTR,
  LBLINT
} label_type;

typedef struct transition {
  label_type tpe;
  label lbl;
  int src;
  int dst;
} transition;

int state_len;
int trans_len;
transition* trans_val;
partition* state_partition;

void hopcroft_init();
void hopcroft_free();
void hopcroft_reduce();
void hopcroft_print();
