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
 
 if (argc < 3) {
    printf("Usage: <prob> <testsize> <djkstr> \n");
    return 1;
  }

 int size = atoi(argv[1]);
 double prob = atof(argv[2]);
 int djkstr = atoi(argv[3]);

 srand(time(NULL));

 graph* g = create_graph(size, prob);

 // graph_to_dot(g, "graph.dot");

 item* items[size];

 int i;
 clock_t start;
 long elapsed;

 for (int k = 0; k < 0; k++) {
   
   for (i = 0; i <= 3; i++) {

     heap_type(i);
     make_heap();
     
     start = clock();
     
     dijkstra(g, 0, items);
     
     elapsed = clock() - start;
     
     // run a dummy, i do not know why
     if (k > 0) {     
       printf("%i %i %i %i %ld\n", size, (int)(prob * 10), i, k, elapsed);
     }
     
     //       print(items, size);
     
     if (djkstr == 1) {
       
       while (find_min() != NULL) {
         delete_min();
       }
     }
     
     clear();
     
     for (int i = 0; i < size; i++) {
       free(items[i]);
     }
   }
 }
}
