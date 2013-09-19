#include <stdlib.h>
#include <stdio.h>
#include "heap.h"

int main() {
  heap* h = make_heap();

  for (int k = 0; k < 100; k++) {
    item* i = (item*)malloc(sizeof(item));
    i->key = k;
    i->value = NULL;
    insert(i, h);
  }
  puts("inserts DONE");

  item* j;

  do {
    j = delete_min(h); 
    if (j != NULL) {
      puts("removed one!");
      printf("%i\n", j->key);
    }
  } 
  while (j != NULL);
  
  return 0;
}
