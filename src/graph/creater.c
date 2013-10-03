
#include <stdlib.h>
#include <stdio.h>
#include "dijkstra.h"
#include "graph.h"
#include <time.h>

int main(int argc, char* argv[]) {

  if (argc > 1) {

    int prob = atoi(argv[1]);
    int size = atoi(argv[2]);
    char filename[50];
    sprintf(filename, "%i_%i.gra", prob, size);
    Graph* g = from_file(filename, size);
    sprintf(filename, "%i_%i.dot", prob, size);
    to_dot(g, filename);

  } else {

    srand(time(NULL));
    
    int max_size = 1000;
    int start_size = 10;
    
    puts("creating");
    double prob = 0.1;
    char filename[50];
    for (int i = 1; i < 10; i++) {
      double thisprob = prob * i;
      
      int this_size = start_size;
      while (this_size <= max_size) {
        Graph* g = create(this_size, thisprob);
        sprintf(filename, "%i_%i.gra", (int)(thisprob * 10), this_size);
        to_file(g, filename);
        this_size = this_size * 10;
      }

      printf("%f\n", thisprob);
    }

  }
  
  return 0;
}
