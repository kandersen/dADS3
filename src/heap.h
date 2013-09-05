#ifndef DADS3_HEAP_H
#define DADS3_HEAP_H

struct heap;
struct item;

struct heap* make_heap    ();
       void  insert       (struct item* i, struct heap* h);
struct item* find_min     (struct heap* h);
struct item* delete_min   (struct heap* h);
struct heap* meld         (struct heap* h1, struct heap* h2);
       void  decrease_key (int delta, struct item* i, struct heap* h);
       void  delete       (struct item* i, struct heap* h);

#endif
