#include <stdlib.h>
#include <stdio.h>
#include <time.h>

#include "dijkstra.h"
#include "graph.h"
#include "heaps/heap.h"


void print(item** items, int size) {

  for (int i = 0; i < size; i++) {
    if (items[i] != NULL) {
      printf("%i\n", items[i]->key);
    }
  }
}

int main(int argc, char* argv[]) {
 
  if (argc < 4) {
    printf("Usage: <testsize> <prob> <djkstr> <seed> \n");
    return 1;
  }

  int size = atoi(argv[1]);
  double prob = atof(argv[2]);
  int djkstr = atoi(argv[3]);
  long int seed = atol(argv[4]);
  
  srand(seed);
  
  graph* g = create_graph(size, prob);
  
  // graph_to_dot(g, "graph.dot");
  
  item* items[size];
  
  clock_t start;
  long elapsed;
  
  heap_type(djkstr);
  make_heap();
  
  start = clock();
  
  dijkstra(g, 0, items);
  
  elapsed = clock() - start;
  
  printf("%ld\n", elapsed);
  
  //print(items, size);
}
