#ifndef GRAPH_H
#define GRAPH_H

#include <stdint.h>

struct graph {
  int nodes;
  int * mat;
};

typedef struct graph graph;

graph * create_graph   (int nodes, double prob);
int get_distance       (graph * m, int i, int j);
void set_distance      (graph * m, int i, int j, int val);
void graph_to_dot      (graph * m, char* filename);
void print_graph       (graph * m);
void graph_to_file     (graph* m, char* filename);
graph* graph_from_file (char* filename, int node);

#endif
