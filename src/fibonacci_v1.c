#include <stdlib.h>
#include "heap.h"

typedef struct node node;

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

struct heap { 
  node** forest;
  int size;
  node* min_node;
};

heap* make_heap() {
  return (heap*)malloc(sizeof(heap));
}

void insert(int key, void* i, heap* h) {
  heap* new_h = make_heap();
  new_h->size = 1;
  new_h->forest = (node**)malloc(1000 * sizeof(node*));

  node* n = (node*)malloc(sizeof(node));
  n->key = key;
  n->rank = 0;
  n->marked = 0;
  n->value = i;
  n->parent = NULL;
  n->child = NULL;
  n->left_sibling = n;
  n->right_sibling = n;
  new_h->forest[0] = n;
  new_h->min_node = n;

  *h = *meld(h, new_h);
}

void* find_min (heap* h) {
  return h->min_node->value;
}

heap* meld         (heap* h1, heap* h2) {
  return NULL;
}


void* delete_min   (heap* h);
void  decrease_key (int delta, void* i, heap* h);
void  delete       (void* i, heap* h);
void  to_dot	   (heap* h);

