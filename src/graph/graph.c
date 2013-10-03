#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include "graph.h"

graph* create_graph (int nodes, double prob) {

  graph* newMat = (graph*)malloc(sizeof(graph));
  
  newMat->nodes = nodes;
  newMat->mat = (int*)malloc(nodes * nodes * sizeof(int));

  for (int i = 0; i < nodes; i++) {
    for (int j = 0; j <= i; j++) {
      if (i == j) {
        set_distance(newMat, i, j, 0);
      } else {
        double r = ((double) rand() / (RAND_MAX));
        if (r >= prob) {
          double r2 = ((double) rand() / (RAND_MAX)) + 1;
          set_distance(newMat, i, j, 100 * r2);
          set_distance(newMat, j, i, 100 * r2);
        } else {
          set_distance(newMat, i, j, 0);
          set_distance(newMat, j, i, 0);
        }
      }
    }
  }

  return newMat;
}

int addr(graph * m, int i, int j) {
  return m->nodes * j + i;
}

int get_distance(graph * m, int i, int j) {
  return m->mat[addr(m, i , j)];
}

void set_distance(graph * m, int i, int j, int val) {
  m->mat[addr(m, i, j)] = val;
}

void graph_to_dot(graph * m, char* filename) {

  FILE * out_file = fopen(filename, "w");

  if (out_file == NULL) {
    fprintf(stderr, "Error, couldn't open file: %s!", filename);
    return;
  }

  fprintf(out_file, "graph {\n");

  for (int i = 0; i < m->nodes; i++) {
    for (int j = 0; j < i; j++) {
      int dist = get_distance(m, i, j);
      if (dist > 0) {
        fprintf(out_file, "%i -- %i [label=\"%d\"]\n", i, j, dist);     
      }
    }
  }

  fprintf(out_file, "}\n");
  fclose(out_file);
}

void print_graph (graph * m) {

  for (int i = 0; i < m->nodes; i++) {
    for (int j = 0; j < m->nodes; j++) {
      int edge = get_distance(m, i, j);
      if (edge > 0) {
        printf("%i %i %i\n", i, j, edge);
      }
    }
  }
}

void graph_to_file(graph* m, char* filename) {

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

  fclose(out_file);
}

graph* graph_from_file(char* filename, int nodes) {
  
  graph* newMat = (graph*)malloc(sizeof(graph));
  
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

  fclose(pFile);

  return newMat;
}
