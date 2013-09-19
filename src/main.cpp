#include <stdlib.h>
#include <stdio.h>
#include "heap.h"

int main() {
  char* file = "test.dot";
  heap* h = make_heap();

  for (int k = 0; k < 4; k++) {
    item* i = (item*)malloc(sizeof(item));
    i->key = k;
    i->value = NULL;
    insert(i, h);
  }


    delete_min(h);

  to_dot(h, file);
  


  return 0;
}
