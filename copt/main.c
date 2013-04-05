#include <stdlib.h>
#include <stdio.h>
#include "hopcroft.h"

void print_reduced() {
  int i, len;
  int* ans = partition_contents(state_partition);
  len = partition_length(state_partition);
  printf("=======================================\n");
  fflush(stdout);
  for (i = 0; i < len; i++) {
    printf ("%i ~ %i\n", i, ans[i]);
    fflush(stdout);
  }
}

int main () {

  state_len = 5;
  trans_len = 4;
  trans_val = calloc(trans_len, sizeof(transition));

  (trans_val + 0)->tpe = LBLATM;
  (trans_val + 0)->lbl.lbl_atm.intfield_num = 18;
  (trans_val + 0)->lbl.lbl_atm.intfield_val = 42;
  (trans_val + 0)->src = 0;
  (trans_val + 0)->dst = 0;

  (trans_val + 1)->tpe = LBLPTR;
  (trans_val + 1)->lbl.lbl_ptr = 0;
  (trans_val + 1)->src = 2;
  (trans_val + 1)->dst = 0;

  (trans_val + 2)->tpe = LBLTAG;
  (trans_val + 2)->lbl.lbl_tag = 42;
  (trans_val + 2)->src = 1;
  (trans_val + 2)->dst = 1;

  (trans_val + 3)->tpe = LBLPTR;
  (trans_val + 3)->lbl.lbl_ptr = 0;
  (trans_val + 3)->src = 3;
  (trans_val + 3)->dst = 1;

  hopcroft_init();
  hopcroft_print();
  hopcroft_reduce();
  print_reduced();
  hopcroft_free();
  free(trans_val);
  return EXIT_SUCCESS;
}
