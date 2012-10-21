#include <stdio.h>
#include <stdlib.h>
#include "partition.h"

struct partition {
  int size;
  /*  Size of the variable length array */
  int length;
  /*  Number of elements */
  int plength;
  /*  Number of partitions */
  int* first;
  /*  Index of the first element in a partition */
  int* last;
  /*  Successor index of the last element in a partition */
  int* marked;
  /*  Index of the last marked element of a partition */
  int* index;
  /*  Partition associated to an element */
  int* elements;
  /*  Elements of a partition */
  int* location;
  /*  Location of an element in [elements] */
};

partition* partition_init (int size, int length) {
  partition* p;
  int i;
  p = malloc(sizeof(partition));
  p->size = size;
  p->length = length;
  p->plength = 1;
  p->first = calloc(size, sizeof(int));
  p->last = calloc(size, sizeof(int));
  p->marked = calloc(size, sizeof(int));
  p->index = calloc(length, sizeof(int));
  p->elements = calloc(length, sizeof(int));
  p->location = calloc(length, sizeof(int));
  /*  Init inner parts */
  p->first[0] = 0;
  p->last[0] = length;
  p->marked[0]= 0;
  for (i = 0; i < length; i++) {
    p->index[i] = 0;
    p->elements[i] = i;
    p->location[i] = i;
  };
  return p;
}

void partition_free (partition* p) {
  free(p->first);
  free(p->last);
  free(p->marked);
  free(p->index);
  free(p->elements);
  free(p->location);
  free(p);
}

void partition_resize (partition* p) {
  int nlen;
  if (p->size < p->plength) {
    nlen = 2 * p->plength + 1;
    p->first = realloc(p->first, nlen * sizeof(int));
    p->last = realloc(p->last, nlen * sizeof(int));
    p->marked = realloc(p->marked, nlen * sizeof(int));
  }
}

int partition_split(partition* p, set s) {
  int i, len;
/*  assert (
    s < p->plength &&
    p->marked[s] <= p->last[s] &&
    p->first[s] <= p->marked[s]
  ); */
  if (p->marked[s] == p->last[s]) p->marked[s] = p->first[s];
  if (p->marked[s] == p->first[s]) return (-1);
  len = (p->plength)++;
  partition_resize(p);
  p->first[len] = p->first[s];
  p->marked[len] = p->first[s];
  p->last[len] = p->marked[s];
  p->first[s] = p->marked[s];
  for (i = p->first[len]; i < p->last[len]; i++) {
    p->index[p->elements[i]] = len;
  }
  return len;
}

void partition_mark(partition* p, int i) {
  set s = p->index[i];
  int loc = p->location[i];
  int mark = p->marked[s];
  if (mark <= loc) {
    p->elements[loc] = p->elements[mark];
    p->location[p->elements[loc]] = loc;
    p->elements[mark] = i;
    p->location[i] = mark;
    p->marked[s] = mark + 1;
  }
}

int partition_is_marked(partition* p, set s) {
  return (p->marked[s] != p->first[s]);
}

int partition_length(partition* p) {
  return p->length;
}

int partition_cardinal(partition* p) {
  return p->plength;
}

int* partition_contents(partition* p) {
  return p->index;
}

int partition_choose(partition* p, set s) {
  return p->first[s];
}

int partition_size(partition* p, set s) {
  return (p->last[s] - p->first[s]);
}

int partition_first(partition* p, set s) {
  return (p->elements[p->first[s]]);
}

int partition_next(partition* p, set s, int i) {
  int loc = p->location[i] + 1;
  if (p->last[s] <= loc) return (-1);
  return p->elements[loc];
}

void partition_print(partition* p) {
  int i;
  for (i = 0; i < p->length; i++) {
    printf("%i ", p->elements[i]);
  }
  printf("|| ");
  for (i = 0; i < p->plength; i++) {
    printf("[%i -> %i] ", p->first[i], p->last[i]);
  }
  printf("\n");
}
