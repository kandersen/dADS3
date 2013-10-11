#include <time.h>
#include <stdlib.h>
#include <stdio.h>
»
#include "dijkstra.h"
#include "graph.h"

int main(int argc, char* argv[]) {

  if (argc == 5) {

    double prob = atof(argv[2]);
    int size = atoi(argv[1]);
    long int seed = atol(argv[3]);

    srand(seed);
    graph* g = create_graph(size, prob);
    print_graph(g);
  }
  else if (argc == 4) {
    char filename[50];
    sprintf(filename, "%s.gra", argv[2]);
    graph* g = graph_from_file(filename, atoi(argv[3]));
    sprintf(filename, "%s.dot", argv[2]);
    graph_to_dot(g, filename);
  } 
  else if (argc == 3) {
    double prob = atof(argv[2]);
    int size = atoi(argv[1]);

    srand(time(NULL));
    graph* g = create_graph(size, prob);
    print_graph(g);
  } else if (argc == 2) {
    printf("%lu", time(NULL));
  } else {

    puts("usage: \t print <graphname> <size> prints dot version");
    puts("\t <size> <prob> makes new graph");
  } 

  
  return 0;
}
