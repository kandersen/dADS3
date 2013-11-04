#ifndef GRAPH_H
#define GRAPH_H

#include <stdint.h>

typedef struct graph graph;
typedef struct g_node g_node;
typedef struct edge edge;

struct edge {
  int distance;
  g_node* target;
};

struct g_node {
  int id;
  int edge_count;
  edge** edges;
  int visited;
};

struct graph {
  int node_count;
  struct g_node** nodes;
};


graph * create_graph   (int nodes, double prob);
void graph_to_dot      (graph * m, char* filename);
void print_graph       (graph * m);
void graph_to_file     (graph* m, char* filename);
graph* graph_from_file (char* filename, int node);

#endif
