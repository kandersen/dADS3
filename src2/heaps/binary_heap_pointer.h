#ifndef DADS3_BINARY_HEAP_POINTER_H
#define DADS3_BINARY_HEAP_POINTER_H
#include "heap.h"

struct bhp_heap;
struct bhp_node;
typedef struct bhp_node bhp_node;
typedef struct bhp_heap bhp_heap;

bhp_heap* bhp_make_heap     ();
void  bhp_insert_item   (item* i, bhp_heap* h);
item* bhp_find_min      (bhp_heap* h);
item* bhp_delete_min    (bhp_heap* h);
bhp_heap* bhp_meld          (bhp_heap* h1, bhp_heap* h2);
void  bhp_decrease_key  (int delta, item* i, bhp_heap* h);
void  bhp_remove_item   (item* i, bhp_heap* h);
void  bhp_to_dot	    (bhp_heap* h, char* filename);
int   bhp_is_empty      (bhp_heap* h);
int   bhp_count         (bhp_heap* h);
bhp_heap* bhp_make_queue    (void* items[], int keys[], int count);
int   bhp_is_consistent (bhp_heap* h);

#endif
