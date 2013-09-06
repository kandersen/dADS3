#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "heap.h"

typedef struct node node;

struct node { 
  node* left;
  node* right;
  int   key;
  void* value;
};

node* new_node(int key, void* value) {
  node* n = malloc(sizeof(node));
  n->key = key;
  n->value = value;
  n->left = 0;
  n->right = 0;
  return n;
}

struct heap {
  node* root;
  int count;
};

heap* make_heap() {
  heap* h = malloc(sizeof(heap));
  h->root = 0;
  h->count = 0;
  return h;
}

void  insert (int key, void* i, heap* h) {
  node* insertee = new_node(key, i);
  if(h->count == 0) {
    h->root = insertee;
    h->count = 1;
  } else {
    h->count = h->count + 1;
    int depth = (int) log2(h->count);
    node* visited[depth + 1];
    int path = h->count;
    node* parent = h->root;
    visited[depth] = parent;

    for (int i = depth - 1; i > 0; --i) {
      if ((path >> i) & 1) {
	parent = parent->right;
      } else {
	parent = parent->left;
      }
      visited[i] = parent;
    }

    if(path & 1) {
      parent->right = insertee;
    } else {
      parent->left = insertee;
    }
    
    visited[0] = insertee;

    for (int i = 0; i < depth; i++) {
      if (visited[i]->key < visited[i+1]->key) {
	int temp_key = visited[i]->key;
	void* temp_value = visited[i]->value;
	visited[i]->key = visited[i+1]->key;
	visited[i]->value = visited[i+1]->value;
	visited[i+1]->key = temp_key;
	visited[i+1]->value = temp_value;
      } else {
	break;
      }
    }
  }
};

void* find_min (heap* h) {
  return h->root->value;
}

void* delete_min (heap* h);
heap* meld         (heap* h1, heap* h2);
void  decrease_key (int delta, void* i, heap* h);
void  delete       (void* i, heap* h);

int is_empty(heap* h) {
  return h->count == 0;
}

int count (heap* h) {
  return h->count;
}

// TESTS
typedef struct foo {
  int v;
} foo;

foo* new_foo(int i) {
  foo* f = (foo*) malloc(sizeof(foo));
  f->v = i;
  return f;
}

int main() {
  heap* h = make_heap();
  for(int i = 18; i > 0; --i) {
    insert(i, new_foo(i), h);
  }

  printf("Hello, %i!\n", ((foo*)find_min(h))->v);
  //  printf("Hello, %i!\n", ((foo*)delete_min(h))->v);
  //  printf("Hello, %i!\n", ((foo*)delete_min(h))->v);
  //  printf("Hello, %i!\n", ((foo*)delete_min(h))->v);
  return 0;
}
