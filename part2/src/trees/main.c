#include <stdlib.h>
#include <stdio.h>
#include <time.h>

#include "heap.h"
#include "dijkstra.h"
#include "graph.h"


int main(int argc, char* argv[]) {
 
 if (argc < 4) {
    printf("Usage: <prob> <testsize> <seed>\n");
    return 1;
  }

 double prob = atof(argv[1]);
 int size = atoi(argv[2]);
 long int seed = atol(argv[3]);

 srand(seed);

 graph* g = create_graph(size, prob);

 clock_t start = clock(), elapsed = 0;

 item* items[size];

 dijkstra(g, 0, items);

 elapsed = clock() - start;
  
 /*   for (int i = 0; i < size; i++) {
   if (items[i] != NULL) {
     printf("%i\n", items[i]->key);
   }
   } */
 
 printf("%ld\n", elapsed);
}
