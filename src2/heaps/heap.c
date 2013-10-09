#include "heap.h"
#include "binary_heap_array.h"
#include "binary_heap_pointer.h"
#include "fibonacci_v1.h"
#include "fibonacci_v2.h"
#include <stdlib.h>
#include <stdio.h>

bha_heap* bha_h;
bhp_heap* bhp_h;
fh1_heap* fh1_h;
fh2_heap* fh2_h;

int this_type;

void heap_type(int t) {
  this_type = t;
}

void 	make_heap	() {

  if (this_type == 0) {
    bha_h = bha_make_heap();
  } else if (this_type == 1) {
    bhp_h = bhp_make_heap();
  } else if (this_type == 2) {
    fh1_h = fh1_make_heap();
  } else {
    fh2_h = fh2_make_heap();
  }

}

void 	insert_item	(item* i) {
  if (this_type == 0) {
    bha_insert_item(i, bha_h);
  } else if (this_type == 1) {
    bhp_insert_item(i, bhp_h);
  } else if (this_type == 2) {
    fh1_insert_item(i, fh1_h);
  } else {
    fh2_insert_item(i, fh2_h);
  }
}

item*	find_min	() {
  if (this_type == 0) {
    return bha_find_min(bha_h);
  } else if (this_type == 1) {
    return bhp_find_min(bhp_h);
  } else if (this_type == 2) {
    return fh1_find_min(fh1_h);
  } else {
    return fh2_find_min(fh2_h);
  }
}

item*	delete_min	() {
  if (this_type == 0) {
    return bha_delete_min(bha_h);
  } else if (this_type == 1) {
    return bhp_delete_min(bhp_h);
  } else if (this_type == 2) {
    return fh1_delete_min(fh1_h);
  } else {
    return fh2_delete_min(fh2_h);
  }
}

void	decrease_key	(int delta, item* i) {
  if (this_type == 0) {
    bha_decrease_key(delta, i, bha_h);
  } else if (this_type == 1) {
    bhp_decrease_key(delta, i, bhp_h);
  } else if (this_type == 2) {
    fh1_decrease_key(delta, i, fh1_h);
  } else {
    fh2_decrease_key(delta, i, fh2_h);
  }
}

void	remove_item	(item* i) {
  if (this_type == 0) {
    bha_remove_item(i, bha_h);
  } else if (this_type == 1) {
    bhp_remove_item(i, bhp_h);
  } else if (this_type == 2) {
    fh1_remove_item(i, fh1_h);
  } else {
    fh2_remove_item(i, fh2_h);
  }
}

void	to_dot		(char* filename) {
  if (this_type == 0) {
    bha_to_dot(bha_h, filename);
  } else if (this_type == 1) {
    bhp_to_dot(bhp_h, filename);
  } else if (this_type == 2) {
    fh1_to_dot(fh1_h, filename);
  } else {
    fh2_to_dot(fh2_h, filename);
  }
}

int 	is_empty	() {
  if (this_type == 0) {
    return bha_is_empty(bha_h);
  } else if (this_type == 1) {
    return bhp_is_empty(bhp_h);
  } else if (this_type == 2) {
    return fh1_is_empty(fh1_h);
  } else {
    return fh2_is_empty(fh2_h);
  }
}

void clear() {
  if (this_type == 0) {
    bha_clear(bha_h);
    free(bha_h);
  } else if (this_type == 1) {
    free(bhp_h);
  } else if (this_type == 2) {
    free(fh1_h);
  } else {
    free(fh2_h);
  }
}
