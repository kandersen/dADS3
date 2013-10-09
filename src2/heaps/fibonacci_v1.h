#ifndef DADS3_FIBONACCI_HEAP_V1_H
#define DADS3_FIBONACCI_HEAP_V1_H
#include "heap.h"

struct fh1_heap;
struct fh1_node;
typedef struct fh1_node fh1_node;
typedef struct fh1_heap fh1_heap;

fh1_heap* 	fh1_make_heap     ();
void  		fh1_insert_item   (item* i, fh1_heap* h);
item* 		fh1_find_min      (fh1_heap* h);
item* 		fh1_delete_min    (fh1_heap* h);
fh1_heap* 	fh1_meld          (fh1_heap* h1, fh1_heap* h2);
void  		fh1_decrease_key  (int delta, item* i, fh1_heap* h);
void  		fh1_remove_item   (item* i, fh1_heap* h);
void  		fh1_to_dot	  (fh1_heap* h, char* filename);
int   		fh1_is_empty      (fh1_heap* h);
int   		fh1_count         (fh1_heap* h);
fh1_heap* 	fh1_make_queue    (void* items[], int keys[], int count);
int   		fh1_is_consistent (fh1_heap* h);

#endif
