#include <stdlib.h>
#include "heap.h"

struct node {
  int key;
  int rank;
  int marked;
  void* value;
  node* parent;
  node* child;
  node* left_sibling;
  node* right_sibling;
};

typedef struct node node;

struct heap { 
  node** forest;
  int size;
};

heap* make_heap() {
  return (heap*)malloc(sizeof(heap));
}



