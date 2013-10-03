#include <stdlib.h>
#include <stdio.h>
#include "heap.h"
#include "dijkstra.h"
#include "graph.h"
#include <time.h>

int main(int argc, char* argv[]) {
 
 if (argc < 3) {
    printf("Usage: <prob> <testsize>\n");
    return 1;
  }

 int size = atoi(argv[2]);

 char filename[50];
 sprintf(filename, "%s_%i.gra", argv[1], size);

 Graph* g = from_file(filename, size);

 clock_t start = clock(), elapsed = 0;

 item* items[size];

 dijkstra(g, 0, items);

 if (elapsed == 0)
   elapsed = clock() - start;

 for (int i = 0; i < size; i++) {
   if (items[i] != NULL) {
     printf("%i\n", items[i]->key);
   }
 }
 
 printf("%ld\n", elapsed);
}
