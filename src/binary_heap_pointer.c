#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "heap.h"

//TYPES

struct node { 
  item* left;
  item* right;
  item* parent;
};

struct heap {
  item* root;
  int count;
};

//UTILITY

node* new_node() {
  node* n = (node*) malloc(sizeof(node));
  n->left = NULL;
  n->right = NULL;
  n->parent = NULL;
  return n;
}

//INTERFACE

heap* make_heap() {
  heap* h = (heap*) malloc(sizeof(heap));
  h->root = NULL;
  h->count = 0;
  return h;
}

void insert (item* k, heap* h) {
  k->n = new_node();
  if(h->count == 0) {
    h->root = k;
    h->count = 1;
  } else {
    h->count = h->count + 1;

    int depth = (int) log2(h->count);

    int path = h->count;
    item* parent = h->root;
    
    for (int i = depth - 1; i > 0; --i) {
      if ((path >> i) & 1) {
	parent = parent->n->right;
      } else {
	parent = parent->n->left;
      }
    }

    if(path & 1) {
      parent->n->right = k;
    } else {
      node* foo =  parent->n;
      foo->left = k;
    }

    k->n->parent = parent;

    item* current = k;
    while(current->n->parent) {
      item* parent = current->n->parent;
      if(parent->key <= current->key) {
	break;
      } else {
	parent->n->left = current->n->left;
	if(parent->n->left) {
	  parent->n->left->n->parent = parent;
	}

	item* parentright_temp = parent->n->right;
	parent->n->right = current->n->right;
	if(parent->n->right) {
	  parent->n->right->n->parent = parent;
	}
	current->n->right = parentright_temp;
	if(parent->n->right) {
	  current->n->right->n->parent = parent;
	}

	if (parent->n->parent != NULL) {
	  if (parent->n->parent->n->left == parent) {
	    parent->n->parent->n->left = current;
	  } else {
	    parent->n->parent->n->right = current;
	  }
	}

	current->n->parent = parent->n->parent;
	parent->n->parent = current;

	if (current->n->right != NULL) {
	  current->n->right->n->parent = current;
	}
      } 
    }
  }
}

item* find_min (heap* h) {
  return h->root;
}

/*
item* delete_min (heap* h) {
  item* result = h->root;
  if(h->count == 1) {
    free(h->root);
    h->count = 0;
    return result;
  }
  
  int path = h->count;
  item* parent = h->root;
  
  for (int i = log2(path) - 1; i > 0; --i) {
    if ((path >> i) & 1) {
      parent = parent->n->right;
    } else {
      parent = parent->n->left;
    }
  }
  
  item* last_node;
  if(path & 1) {
    last_node = parent->n->right;
    parent->n->right = NULL;
  } else {
    last_node = parent->n->left;
    parent->n->left = NULL;
  }
  
  h->root->key = last_node->key;
  h->root->value = last_node->value;
  free(last_node);
  h->count = h->count - 1;

  parent = h->root;
  while(parent->n->left) {
    if(parent->n->right) {
      if(parent->key <= parent->n->left->key && parent->key <= parent->n->right->key) {
	break;
      } else if(parent->n->left->key <= parent->key && parent->n->left->key <= parent->n->right->key) {
	swap_nodes(parent, parent->n->left);
	parent = parent->n->left;
      } else {
	swap_nodes(parent, parent->n->right);
	parent = parent->n->right;
      }
    } else {
      if (parent->key >= parent->n->left->key) {
	swap_nodes(parent, parent->n->left);
      } 
      break;
    }
  }
  return result;
}

int is_empty(heap* h) {
  return h->count == 0;
}

int count (heap* h) {
  return h->count;
}
*/

//TESTING CODE

typedef struct foo {
  int v;
} foo;

foo* new_foo(int i) {
  foo* f = (foo*) malloc(sizeof(foo));
  f->v = i;
  return f;
}

item* new_item(void* value, int key) {
  item* i = (item*) malloc(sizeof(item));
  i->value = value;
  i->key = key;
  i->n = 0;
  return i;
}


int main() {
  heap* h = make_heap();
  item* t = new_item(new_foo(9), 9);

  insert(t, h);
  insert(new_item(new_foo(4), 4), h);
  insert(new_item(new_foo(1), 1), h);
  //insert(new_item(new_foo(7), 7), h);
  /* insert(new_item(new_foo(5), 5), h); */
  /* insert(new_item(new_foo(5), 5), h); */
  /* insert(new_item(new_foo(2), 2), h); */
  //   insert(new_item(new_foo(0), 0), h);
  /* insert(new_item(new_foo(8), 8), h); */
  /* insert(new_item(new_foo(3), 3), h); */

  //decrease_key(10, t, h);
  for(int i = 0; i < 10; i++) {
    printf("Hello, %i!\n", ((foo*)find_min(h))->v);
  }
  return 0;
}

