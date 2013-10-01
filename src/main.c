#include <stdlib.h>
#include <stdio.h>
#include "heap.h"
#include "dijkstra.h"
#include "graph.h"

int main() {
  
  int nodes = 100;
  puts("creating");
  Graph* g = create(nodes, 0.9);
  puts("to_file");
  to_file(g, "1000.txt");
  puts("from_file");
  Graph* g2 = from_file("1000.txt", nodes);
  puts("to_dot");
  to_dot(g2, "1000.dot");
  
  return 0;
}
