#include "heap.h"
#include <stdlib.h>
#include <stdio.h>
#include "dijkstra.h"
#include "graph.h"

void dijkstra(Graph* g, int source, item** ph) {
  
  int infinity = 100000000;
  int vertices = g->nodes;

  heap* h = make_heap();

  for (int i = 0; i < vertices; i++) {
    item* itm = (item*)malloc(sizeof(item));
    if (i == source) {
      itm->key = 0;
    } else {
      itm->key = infinity;    
    }
    int* item_val = (int*)malloc(sizeof(int));
    *item_val = i;
    itm->value = item_val;
    
    ph[i] = itm;
    insert(itm, h);
  }

  item* val = delete_min(h);

  while (val != NULL) {
    
    if (val->key == infinity) {
      break;
    }

    int u_index = *((int*)(val->value));

    for (int v_index = 0; v_index < vertices; v_index++) {
      int dist_between = get(g, u_index, v_index);
      if (dist_between > 0) { // it is a neighbor
        item* v = ph[v_index];
        int alt = val->key + dist_between;
        if (alt < v->key) {
          int delta = v->key - alt;
          decrease_key(delta, v, h);
        }
      }
    }

    val = delete_min(h);
  }
}
