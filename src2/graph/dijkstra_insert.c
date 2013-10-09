
#include <stdlib.h>
#include <stdio.h>
#include "heap.h"
#include "dijkstra.h"
#include "graph.h"

void dijkstra(graph* g, int source, item** ph) {
  
  int infinity = 100000000;
  int vertices = g->nodes;

  make_heap();

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
    insert_item(itm);
  }

  item* val = find_min();
  delete_min();
  
  while (val != NULL) {

    if (val->visited == 1) {
      break;
    } else {
      val->visited = 1;
    }

    int u_index = *((int*)(val->value));    

    for (int v_index = 0; v_index < vertices; v_index++) {
      int dist_between = get_distance(g, u_index, v_index);
      if (dist_between > 0) { // it is a neighbor
        item* v = ph[v_index];
        int alt = val->key + dist_between;
        item* newitem = (item*)malloc(sizeof(item));
        if (v->key > alt) {
          newitem->key = alt;
          newitem->visited = v->visited;
          newitem->value = v->value;
          insert_item(newitem);
          ph[v_index] = newitem;
        }
      }
    }

    val = find_min();
    delete_min();
  }
}
