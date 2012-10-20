#include <stdlib.h>
#include <stdio.h>
#include "buffer.h"
#include "partition.h"
#include "hopcroft.h"

#define BUFFER_SIZE 97

static partition* trans_partition;
static buffer** trans_reverse;

static buffer* state_touched;
static buffer* trans_waiting;
static buffer* trans_touched;

int strlbl_cmp(label p1, label p2) {
  int i;
  string s1 = p1.lbl_str;
  string s2 = p2.lbl_str;
  if (s1.string_len != s2.string_len)
    return (s1.string_len - s2.string_len);
  for (i = 0; i < s1.string_len; i++) {
    if (s1.string_ptr[i] != s2.string_ptr[i])
      return (s1.string_ptr[i] - s2.string_ptr[i]);
  }
  return 0;
}

int intlbl_cmp(label p1, label p2) {
  intfield f1 = p1.lbl_int;
  intfield f2 = p2.lbl_int;
  if (f1.intfield_num != f2.intfield_num)
    return (f1.intfield_num - f2.intfield_num);
  return (f1.intfield_val - f2.intfield_val);
}

int trans_cmp(const void* p1, const void* p2) {
  transition* t1 = (transition*) p1;
  transition* t2 = (transition*) p2;
  if (t1->tpe != t2->tpe) return (t1->tpe - t2->tpe);
  switch (t1->tpe) {
    case LBLSTR:
      return (strlbl_cmp(t1->lbl, t2->lbl));
    case LBLTAG:
      return (t1->lbl.lbl_tag - t2->lbl.lbl_tag);
    case LBLPTR:
      return (t1->lbl.lbl_ptr - t2->lbl.lbl_ptr);
    case LBLINT:
      return (intlbl_cmp(t1->lbl, t2->lbl));
    case LBLATM:
      return (t1->lbl.lbl_atm - t2->lbl.lbl_atm);
    default:
      return 0;
  }
}

void trans_print(transition* t) {
  printf ("%i -> %i :: ", t->src, t->dst);
  switch (t->tpe) {
    case LBLSTR:
      printf ("STR _\n");
      return;
    case LBLTAG:
      printf ("TAG %i\n", t->lbl.lbl_tag);
      return;
    case LBLPTR:
      printf ("PTR %i\n", t->lbl.lbl_ptr);
      return;
    case LBLINT:
      printf ("INT %i %i\n", t->lbl.lbl_int.intfield_num, t->lbl.lbl_int.intfield_val);
      return;
    case LBLATM:
      printf ("ATM %i\n", t->lbl.lbl_atm);
      return;
    default:
      return;
  }
}

void hopcroft_reverse_init() {
  int i, dst;
  trans_reverse = calloc(state_len, sizeof(buffer*));
  for (i = 0; i < state_len; i++) {
    trans_reverse[i] = buffer_init(BUFFER_SIZE);
  }
  for (i = 0; i < trans_len; i++) {
    dst = (trans_val + i)->dst;
    buffer_push(trans_reverse[dst], i);
  }
}

void hopcroft_split_init() {
  int i;
  transition t;
  if (trans_len <= 0) return;
  t = trans_val[0];
  for (i = 0; i < trans_len; i++) {
    if (trans_cmp(&t, trans_val + i) != 0) {
      partition_split(trans_partition, 0);
      t = trans_val[i];
    }
    partition_mark(trans_partition, i);
  }
  partition_split(trans_partition, 0);
  /* Push transition partitions */
  for (i = 0; i < partition_cardinal(trans_partition); i++) {
    buffer_push(trans_waiting, i);
  }
}

void hopcroft_init() {
  state_partition = partition_init(97, state_len);
  trans_partition = partition_init(97, trans_len);
  state_touched = buffer_init(state_len);
  trans_waiting = buffer_init(trans_len);
  trans_touched = buffer_init(trans_len);
  /* Sort transitions according to their label */
  qsort(trans_val, trans_len, sizeof(transition), &trans_cmp);
  /* Split transitions according to their label */
  hopcroft_split_init();
  /* Compute reverse pointers */
  hopcroft_reverse_init();
}

void hopcroft_split(set s) {
  int i, r, state, trans, len, split;
  r = partition_split(state_partition, s);
  buffer_clear(trans_touched);
  if (0 <= r) {
    /* Take the minimal partition */
    if (partition_size(state_partition, s) < partition_size(state_partition, r))
      r = s;
    state = partition_first(state_partition, r);
    while (0 <= state) {
      len = buffer_length(trans_reverse[state]);
      for (i = 0; i < len; i++) {
        trans = buffer_contents(trans_reverse[state])[i];
        split = partition_contents(trans_partition)[trans];
        if (!partition_is_marked(trans_partition, split))
          buffer_push(trans_touched, split);
        partition_mark(trans_partition, trans);
      }
      state = partition_next(state_partition, r, state);
    }
    len = buffer_length(trans_touched);
    for (i = 0; i < len; i++) {
      split = buffer_contents(trans_touched)[i];
      split = partition_split(trans_partition, split);
      if (0 <= split) buffer_push (trans_waiting, split);
    }
  }
}

void hopcroft_reduce() {
  int prev, partprev, trans, split, len, state;
  while (0 < buffer_length(trans_waiting)) {
    partition_print(trans_partition);
    partition_print(state_partition);
    split = buffer_pop(trans_waiting);
    trans = partition_first(trans_partition, split);
    while (0 <= trans) {
      prev = (trans_val + trans)->src;
      partprev = partition_contents(state_partition)[prev];
      if (!partition_is_marked(state_partition, partprev))
        buffer_push(state_touched, partprev);
      partition_mark(state_partition, prev);
      trans = partition_next(trans_partition, split, trans);
    }
    len = buffer_length(state_touched);
    for (state = 0; state < len; state++) {
      hopcroft_split(buffer_contents(state_touched)[state]);
    }
    buffer_clear(state_touched);
  }
}

void hopcroft_free() {
  int i;
  partition_free(state_partition);
  partition_free(trans_partition);
  for (i = 0; i < state_len; i++) {
    buffer_free(trans_reverse[i]);
  }
  free(trans_reverse);
  buffer_free(state_touched);
  buffer_free(trans_touched);
  buffer_free(trans_waiting);
}

void hopcroft_print() {
  int i;
  for (i = 0; i < state_len; i++) {
    printf("%i ==> %i\n", i, partition_contents(state_partition)[i]);
    fflush(stdout);
  }
  for (i = 0; i < trans_len; i++) {
    trans_print (trans_val + i); fflush(stdout);
  }
}
