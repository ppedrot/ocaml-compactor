#include <stdio.h>
#include <stdlib.h>
#include "buffer.h"

struct buffer {
  int size;
  int length;
  int* content;
};

buffer* buffer_init(int size) {
  buffer* b = malloc(sizeof(buffer));
  b->size = size;
  b->length = 0;
  b->content = calloc(size, sizeof(int));
  return b;
}

void buffer_resize(buffer* b) {
  int nsize = 2 * b->size + 1;
  b->content = realloc(b->content, nsize * sizeof(int));
  b->size = nsize;
}

void buffer_push(buffer* b, int val) {
  if (b->size <= b->length) buffer_resize(b);
  b->content[b->length] = val;
  b->length += 1;
  return;
}

int buffer_pop(buffer* b) {
  b->length -= 1;
  return (b->content[b->length]);
}

void buffer_free(buffer* b) {
  free(b->content);
  free(b);
}

int buffer_length(buffer* b) {
  return (b->length);
}

int* buffer_contents(buffer* b) {
  return (b->content);
}

void buffer_clear(buffer* b) {
  b->length = 0;
}
