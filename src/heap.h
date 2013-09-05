#ifndef DADS3_HEAP_H
#define DADS3_HEAP_H

struct heap;
struct item;

heap* make_heap    ();
void  insert       (item i, heap h);
item* find_min     (heap h);
item* delete_min   (heap h);
heap* meld         (heap h1, heap h2);
void  decrease_key (int i, item i, heap h);
void  delete       (item i, heap h);

#endif
