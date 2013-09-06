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

void swap_nodes(node* n1, node* n2) {
  int temp_key = n1->key;
  void* temp_value = n1->value;
  n1->key = n2->key;
  n1->value = n2->value;
  n2->key = temp_key;
  n2->value = temp_value;
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
	swap_nodes(visited[i], visited[i+1]);
      } else {
	break;
      }
    }
  }
};

void* find_min (heap* h) {
  return h->root->value;
}

void* delete_min (heap* h) {
  void* result = h->root->value;
  if(h->count == 1) {
    free(h->root);
    h->count = 0;
    return result;
  }
  
  int path = h->count;
  node* parent = h->root;
  
  for (int i = log2(path) - 1; i > 0; --i) {
    if ((path >> i) & 1) {
      parent = parent->right;
    } else {
      parent = parent->left;
    }
  }
  
  node* last_node;
  if(path & 1) {
    last_node = parent->right;
    parent->right = NULL;
  } else {
    last_node = parent->left;
    parent->left = NULL;
  }
  
  h->root->key = last_node->key;
  h->root->value = last_node->value;
  free(last_node);
  h->count = h->count - 1;

  node* n = h->root;
  while(n->left) {
    if(n->right) {
      if(n->key <= n->left->key && n->key <= n->right->key) {
	break;
      } else if(n->left->key <= n->key && n->left->key <= n->right->key) {
	swap_nodes(n, n->left);
	n = n->left;
      } else {
	swap_nodes(n, n->right);
	n = n->right;
      }
    } else {
      if (n->key >= n->left->key) {
	swap_nodes(n, n->left);
      } 
      break;
    }
  }
  return result;
}

void node_to_dot(node* n) {
  if(n->left) {
    printf("    %i -> %i;\n", n->key, n->left->key);
    node_to_dot(n->left);
  } else {
    printf("    %i -> NULL;\n", n->key);
  }
  if(n->right) {
    printf("    %i -> %i;\n", n->key, n->right->key);
    node_to_dot(n->right);
  } else {
    printf("    %i -> NULL;\n", n->key);
  }
}

int to_dot_count = 0;
void to_dot(heap* h) {
  printf("digraph bin_heap_points_%i {\n", to_dot_count++);
  node_to_dot(h->root);
  printf("}\n");
}

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

  int numbers[10] = { 4, 7, 2, 3, 8, 5, 6, 1, 0, 9 };
  for(int i = 0; i < 10; ++i) {
    insert(numbers[i], new_foo(numbers[i]), h);
  }

  for(int i = 0; i < 10; i++) {
    printf("%i ", ((foo*)delete_min(h))->v);
  }
  printf("\n");
  return 0;
}
