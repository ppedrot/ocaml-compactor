#define set int

typedef struct partition partition;

partition* partition_init(int size, int length);

void partition_free(partition* p);

set partition_split(partition* p, set s);

void partition_mark(partition* p, int i);

int partition_is_marked(partition* p, set s);

int partition_length(partition* p);

int partition_cardinal(partition* p);

set* partition_contents(partition* p);

int partition_size(partition* p, set s);

int partition_first(partition* p, set s);
int partition_next(partition* p, set s, int i);

void partition_print(partition* p);
