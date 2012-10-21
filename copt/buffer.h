typedef struct buffer buffer;

/* Buffer storing integers */

buffer* buffer_init(int size);
void buffer_free(buffer* b);
int buffer_pop(buffer* b);
void buffer_push(buffer* b, int i);
int buffer_length(buffer* b);
int* buffer_contents(buffer* b);
void buffer_clear(buffer* b);
