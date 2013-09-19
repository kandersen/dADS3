#include <stdlib.h>
#include <stdio.h>
#include "heap.h"

int main() {

  heap* h = make_heap();

  for (int k = 0; k < 1000; k++) {
    item* i = (item*)malloc(sizeof(item));
    i->key = k;
    i->value = NULL;
    insert(i, h);
  }

  item* j = delete_min(h);

  while (j != NULL) {
    printf("%i\n", j->key);
    j = delete_min(h);
  }

  return 0;
}
