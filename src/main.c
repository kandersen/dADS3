#include <stdlib.h>
#include <stdio.h>
#include "heap.h"
#include "dijkstra.h"
#include "graph.h"

int main() {
  
  int nodes = 1000;

  Graph* g = create(nodes, 0.9);

  to_dot(g, "graph.dot");

  item* result[nodes];

  dijkstra(g, 0, result);

  for (int i = 0; i < nodes; i++) {
    if (result[i] != NULL) {
      printf("to node %i: %i\n", i, result[i]->key);
    }
  }

  return 0;
}
