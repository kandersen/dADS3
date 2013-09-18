#ifndef DADS3_HEAP_H
#define DADS3_HEAP_H

struct item;
struct heap;
typedef struct heap heap;

heap* make_heap    ();
void  insert       (item* i, heap* h);
item* find_min     (heap* h);
item* delete_min   (heap* h);
heap* meld         (heap* h1, heap* h2);
void  decrease_key (int delta, item* i, heap* h);
void  remove       ( item* i, heap* h);
void  to_dot	   (heap* h);
int   is_empty     (heap* h);
int   count        (heap* h);
heap* make_queue   (void* items[], int keys[], int count);

#endif
