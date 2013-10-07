#include <stdlib.h>
#include <stdio.h>
#include <time.h>

#include "heap.h"
#include "dijkstra.h"
#include "graph.h"


int main(int argc, char* argv[]) {
 
 if (argc < 3) {
    printf("Usage: <prob> <testsize>\n");
    return 1;
  }

 int size = atoi(argv[2]);
 char filename[50];
 sprintf(filename, "%s", argv[1]);
 
 graph* g = graph_from_file(filename, size);

 clock_t start = clock(), elapsed = 0;

 item* items[size];

 dijkstra(g, 0, items);

 elapsed = clock() - start;
 
 /* 
  for (int i = 0; i < size; i++) {
   if (items[i] != NULL) {
     printf("%i\n", items[i]->key);
   }
   } */
 
 printf("%ld\n", elapsed);
}
