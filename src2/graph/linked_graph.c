#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include "graph.h"

graph* create_graph (int nodes, double prob) {

  graph* g = (graph*)malloc(sizeof(graph));
  
  g->node_count = nodes;
  g_node** items = (g_node**)malloc(nodes * sizeof(g_node*));
  g->nodes = items;

  for (int i = 0; i < nodes; i++) {
    g_node* n = (g_node*)malloc(sizeof(g_node));
    n->id = i;
    n->edge_count = 0;
    edge** es = (edge**)malloc(nodes * sizeof(edge*));
    n->edges = es;
    n->visited = 0;
    items[i] = n;
  }

  for (int i = 0; i < nodes; i++) {
    for (int j = 0; j < i; j++) {
      double r = ((double) rand() / (RAND_MAX));
      if (r >= prob) {
        double dist = 100 * (((double) rand() / (RAND_MAX)) + 1);
        g_node* n = items[i];
        edge* e = (edge*)malloc(sizeof(edge));
        e->distance = dist;
        e->target = items[j];
        n->edges[n->edge_count] = e;
        n->edge_count += 1;

        g_node* n2 = items[j];
        edge* e2 = (edge*)malloc(sizeof(edge));
        e2->distance = dist;
        e2->target = items[i];
        n2->edges[n2->edge_count] = e2;
        n2->edge_count += 1;
      } 
    }
  }
  

  return g;
}


void graph_to_dot(graph * g, char* filename) {

  FILE * out_file = fopen(filename, "w");

  if (out_file == NULL) {
    fprintf(stderr, "Error, couldn't open file: %s!", filename);
    return;
  }

  fprintf(out_file, "graph {\n");

  for (int i = 0; i < g->node_count; i++) {
    g_node* n = g->nodes[i];
    for (int j = 0; j < n->edge_count; j++) {
      edge* e = n->edges[j];
      fprintf(out_file, "%i -- %i [label=\"%d\"]\n", n->id, e->target->id, e->distance);     
    }
  }

  fprintf(out_file, "}\n");
  fclose(out_file);
}

void print_graph (graph * g) {

  /*
  for (int i = 0; i < g->node_count; i++) {
    g_node* n = g->nodes[i];
    for (int j = 0; j < m->nodes; j++) {
      int edge = get_distance(m, i, j);
      if (edge > 0) {
        printf("%i %i %i\n", i, j, edge);
      }
    }
    } */
}

void graph_to_file(graph* m, char* filename) {
  /*
  FILE * out_file = fopen(filename, "w");

  if (out_file == NULL) {
    fprintf(stderr, "Error, couldn't open file: %s!", filename);
    return;
  }

  for (int i = 0; i < m->nodes; i++) {
    for (int j = 0; j < m->nodes; j++) {

      int edge = get_distance(m, i, j);
      if (edge > 0) {
        fprintf(out_file, "%i %i %i\n", i, j, edge);
      }
    }
  }

  fclose(out_file); */
}

graph* graph_from_file(char* filename, int nodes) {
  
  /*  graph* newMat = (graph*)malloc(sizeof(graph));
  
  newMat->nodes = nodes;
  newMat->mat = (int*)malloc(nodes * nodes * sizeof(int));
  
  char str[100];

  FILE * pFile = fopen (filename , "r");
  if (pFile == NULL) {
    fprintf(stderr, "Error, couldn't open file: %s!", filename);
    return NULL;
  } else {
    int a, b, c;
    while ( fgets(str, 100, pFile) != NULL ) {      
      sscanf(str, "%d %d %d", &a, &b, &c);
      set_distance(newMat, a, b, c);
    }
  }

  fclose(pFile); */

  return NULL;
}

