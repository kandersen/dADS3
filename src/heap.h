#ifndef DADS3_HEAP_H
#define DADS3_HEAP_H

struct heap;
typedef struct heap heap;

heap* make_heap    ();
void  insert       (int key, void* i, heap* h);
void* find_min     (heap* h);
void* delete_min   (heap* h);
heap* meld         (heap* h1, heap* h2);
void  decrease_key (int delta, void* i, heap* h);
void  delete       (void* i, heap* h);


#endif
