
#include <stdlib.h>
#include <stdio.h>
#include "heap.h"
#include "dijkstra.h"
#include "graph.h"

void dijkstra(graph* g, int source, item** ph) {
  
  int infinity = 100000000;
  int vertices = g->node_count;

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
    insert_item(itm, h);
  }

  to_dot(h, "start.dot");

  item* val = find_min(h);
  delete_min(h);
  int counter = 0;
  char fname[50];

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
<<<<<<< HEAD
        newitem->key = alt;
        newitem->value = v->value;
        insert_item(newitem, h);
        ph[target_id] = newitem;
=======
        if (v->key > alt) {
          newitem->key = alt;
          newitem->visited = v->visited;
          newitem->value = v->value;
          insert_item(newitem, h);
          ph[v_index] = newitem;
          sprintf(fname, "%i.dot", counter);
          counter += 1;
          //to_dot(h, fname);
        }
>>>>>>> e325c43fe4d2267d83b8a1313d82aa953128b53b
      }
    }
    
    val = find_min(h);
    delete_min(h);
  }
}
