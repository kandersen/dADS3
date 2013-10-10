
#include <stdlib.h>
#include <stdio.h>
#include "heap.h"
#include "dijkstra.h"
#include "graph.h"

void dijkstra(graph* g, int source, item** ph) {
  
  int infinity = 100000000;
  int vertices = g->node_count;

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

    int u_index = *((int*)(val->value));
    g_node* n = g->nodes[u_index];

    if (n->visited == 1) {
      break;
    } else {
      n->visited = 1;
    }

    for (int v_index = 0; v_index < n->edge_count; v_index++) {

      edge* e = n->edges[v_index];
      int dist_between = e->distance;
      int target_id = e->target->id;
      item* v = ph[target_id];
      int alt = val->key + dist_between;
      if (alt < v->key) {
        item* newitem = (item*)malloc(sizeof(item));
        newitem->key = alt;
        newitem->value = v->value;
        insert_item(newitem);
        ph[target_id] = newitem;
      }
    }
    
    val = find_min();
    delete_min();
  }
}
