#ifndef GRAPH_H
#define GRAPH_H

#include <stdint.h>

struct Graph {
  int nodes;
  int * mat;
};

Graph * create(int nodes, double prob);
int get(Graph * m, int i, int j);
void set(Graph * m, int i, int j, int val);
void to_dot(Graph * m, char* filename);
void print(Graph * m);
void to_file(Graph* m, char* filename);
Graph* from_file(char* filename, int node);

#endif
