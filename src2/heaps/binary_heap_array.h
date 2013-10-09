#ifndef DADS3_BINARY_HEAP_ARRAY_H
#define DADS3_BINARY_HEAP_ARRAY_H
#include "heap.h"

struct bha_heap;
struct bha_node;
typedef struct bha_node bha_node;
typedef struct bha_heap bha_heap;

bha_heap* 	bha_make_heap     ();
void  		bha_insert_item   (item* i, bha_heap* h);
item* 		bha_find_min      (bha_heap* h);
item* 		bha_delete_min    (bha_heap* h);
bha_heap* 	bha_meld          (bha_heap* h1, bha_heap* h2);
void  		bha_decrease_key  (int delta, item* i, bha_heap* h);
void  		bha_remove_item   (item* i, bha_heap* h);
void  		bha_to_dot	  (bha_heap* h, char* filename);
int   		bha_is_empty      (bha_heap* h);
int   		bha_count         (bha_heap* h);
bha_heap* 	bha_make_queue    (void* items[], int keys[], int count);
int   		bha_is_consistent (bha_heap* h);
void  		bha_clear	  (bha_heap* h);
#endif
