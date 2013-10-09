#ifndef DADS3_FIBONACCI_HEAP_V2_H
#define DADS3_FIBONACCI_HEAP_V2_H
#include "heap.h"

struct fh2_heap;
struct fh2_node;
typedef struct fh2_node fh2_node;
typedef struct fh2_heap fh2_heap;

fh2_heap* 	fh2_make_heap     ();
void  		fh2_insert_item   (item* i, fh2_heap* h);
item* 		fh2_find_min      (fh2_heap* h);
item* 		fh2_delete_min    (fh2_heap* h);
fh2_heap* 	fh2_meld          (fh2_heap* h1, fh2_heap* h2);
void  		fh2_decrease_key  (int delta, item* i, fh2_heap* h);
void  		fh2_remove_item   (item* i, fh2_heap* h);
void  		fh2_to_dot	  (fh2_heap* h, char* filename);
int   		fh2_is_empty      (fh2_heap* h);
int   		fh2_count         (fh2_heap* h);
fh2_heap* 	fh2_make_queue    (void* items[], int keys[], int count);
int   		fh2_is_consistent (fh2_heap* h);

#endif
