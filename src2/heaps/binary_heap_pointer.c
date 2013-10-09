#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <string.h>
#include "binary_heap_pointer.h"

//TYPES

struct bhp_node { 
  int key;
  bhp_node* left;
  bhp_node* right;
  bhp_node* parent;
  item* itm;
  char* name;
};

struct bhp_heap {
  bhp_node* root;
  int count;
};

//UTILITY

bhp_node* bhp_new_node(item* itm) {
  bhp_node* n = (bhp_node*) malloc(sizeof(bhp_node));
  n->left = NULL;
  n->right = NULL;
  n->parent = NULL;
  n->itm = itm;
  n->key = itm->key;
  itm->n = n;
  return n;
}


// Swaps either left child or right child of parent and returns the other leg
bhp_node* bhp_swap(bhp_node* n, bhp_node* p) {
  bhp_node* temp;
  
  // OK, we have to swap
  if (p->left == n) {
    // this is left-child
    temp = n->left;
    n->left = p;
    p->left = temp;
    if (temp != NULL)
      temp->parent = p;
    
    temp = p->right;
    p->right = n->right;
    if (p->right != NULL)
      p->right->parent = p;
  
    n->right = temp;
    if (temp != NULL)
      temp->parent = n;

  } else {
    // this is right-child
    temp = n->right;
    n->right = p;
    p->right = temp;
    if (temp != NULL)
      temp->parent = p;
    
    temp = p->left;
    p->left = n->left;
    if (p->left != NULL)
      p->left->parent = p;
  
    n->left = temp;
    if (temp != NULL)
      temp->parent = n;
  }

  if (p->parent != NULL) {
    if (p->parent->left == p) {
      p->parent->left = n;
    } else {
      p->parent->right = n;
    }
  }

  n->parent = p->parent;
  p->parent = n;

  return temp;
}


//INTERFACE

bhp_heap* bhp_make_heap() {
  bhp_heap* h = (bhp_heap*) malloc(sizeof(bhp_heap));
  h->root = NULL;
  h->count = 0;
  return h;
}

void bhp_insert_item (item* k, bhp_heap* h) {
  bhp_node* n = bhp_new_node(k);
  
  if(h->count == 0) {
    h->root = n;
    h->count = 1;
  } else {
    h->count = h->count + 1;

    int depth = (int) log2(h->count);

    int path = h->count;
    bhp_node* parent = h->root;
    
    for (int i = depth - 1; i > 0; --i) {
      if ((path >> i) & 1) {
	parent = parent->right;
      } else {
	parent = parent->left;
      }
    }

    if(path & 1) {
      parent->right = n;
    } else {
      parent->left = n;
    }
    
    n->parent = parent;
    
    while(n->parent != NULL) {
      bhp_node* parent = n->parent;
      if(LE(parent->key, n->key)) {
	break;
      } else {
        bhp_node* new_child = bhp_swap(n, parent);
        if (new_child != NULL && LT(new_child->key, n->key)) {
          bhp_swap(new_child, n);
          if (h->root == parent) {
            h->root = new_child;
          }
          break;
        } else {
          if (h->root == parent) {
            h->root = n;
          } 
        }
      }
    }
  }
}

item* bhp_find_min (bhp_heap* h) {
  if (h->root != NULL) {
    return h->root->itm;
  } else {
    return NULL;
  }
}

item* bhp_delete_min (bhp_heap* h) {

  bhp_node* original_root = h->root;

  if (original_root == NULL)
    return NULL;
  
  if(h->count == 1) {
    item* itm = original_root->itm;
    free(original_root);
    h->count = 0;
    h->root = NULL;
    return itm;
  }
  
  int path = h->count;
  bhp_node* parent = original_root;
  
  for (int i = log2(path) - 1; i > 0; --i) {
    if ((path >> i) & 1) {
      parent = parent->right;
    } else {
      parent = parent->left;
    }
  }  
  
  bhp_node* last_node;
  if(path & 1) {
    last_node = parent->right;
    parent->right = NULL;
  } else {
    last_node = parent->left;
    parent->left = NULL;
  }

  last_node->parent = original_root->parent;
  last_node->left = original_root->left;
  last_node->right = original_root->right;

  if (original_root->left != NULL)
    original_root->left->parent = last_node;

  if (original_root->right != NULL)
    original_root->right->parent = last_node;
  
  bhp_node* to_recurse = NULL;
  bhp_node* new_parent = NULL;
  if (last_node->left != NULL && last_node->right != NULL) {
    if (LT(last_node->left->key, last_node->right->key)) {
      new_parent = last_node->left;
    } else {
      new_parent = last_node->right;
    }
  } else {
    new_parent = last_node->left;    
  } 
  
  if (new_parent != NULL && LT(new_parent->key, last_node->key)) {
    bhp_swap(new_parent, last_node);
    to_recurse = last_node;
    h->root = new_parent;
  } else {
    h->root = last_node;
  }
  
  h->count = h->count - 1;

  while (to_recurse) {
    new_parent = NULL;
    if (to_recurse->left != NULL && to_recurse->right != NULL) {
      if (LT(to_recurse->left->key, to_recurse->right->key)) {
        new_parent = to_recurse->left;
      } else {
        new_parent = to_recurse->right;
      }
    } else if (to_recurse->left != NULL) {
      new_parent = to_recurse->left;
    }
    if (new_parent != NULL && LT(new_parent->key, to_recurse->key)) {
      bhp_swap(new_parent, to_recurse);
    } else {
      to_recurse = NULL;
    }
  }

  item* itm = original_root->itm;
  free(original_root);

  return itm;
}

int bhp_is_empty(bhp_heap* h) {
  return h->count == 0;
}

int bhp_count (bhp_heap* h) {
  return h->count; 
}

void bhp_decrease_key (int delta, item* i, bhp_heap* h) {
  
  i->key = i->key - delta;
  bhp_node* n = i->n;
  n->key = i->key;
  
  while(n->parent != NULL) {
    bhp_node* parent = n->parent;
    if(LE(parent->key, n->key)) {
      break;
    } else {
      bhp_node* new_child = bhp_swap(n, parent);
      if (new_child != NULL && LT(new_child->key, n->key)) {
	bhp_swap(new_child, n);
	if (h->root == parent) {
	  h->root = new_child;
	}
	break;
      } else {
        if (h->root == parent) {
          h->root = n;
        } 
      }
    }
  }
}

void  bhp_remove_item   (item* i, bhp_heap* h) {
  
}

int counter;
void bhp_node_name (bhp_node* n, FILE* out) {
  char buf[10];
  
  fprintf(out, "{ rank=same;\n");
  sprintf(buf, "node%05d", ++counter);    
  n->name = (char*) malloc(sizeof(char)*10);
  strcpy(n->name, buf);
  
  fprintf(out, "%s [label=\"%d\"];\n", n->name, n->key);
  fprintf(out, "}\n");

  if (n->left)
    bhp_node_name(n->left, out);
  if (n->right) 
    bhp_node_name(n->right, out);      
}

void bhp_dot_node (bhp_node* n, FILE* out) {
  if (!n) {
    puts("called bhp_dot_node with null");
    return;
  }

  if (n->left) {
    bhp_dot_node(n->left, out);
    fprintf(out, "%s -> %s [color=\"red\"]\n", n->name, n->left->name);
  }
  
  if (n->right) {
    bhp_dot_node(n->right, out);
    fprintf(out, "%s -> %s [color=\"red\"]\n", n->name, n->right->name);
  }
  
  if (n->parent)
    fprintf(out, "%s -> %s [color=\"blue\"]\n", n->name, n->parent->name);
}
    
void bhp_node_to_dot (bhp_node* n, char* filename) {
  FILE * out_file = fopen(filename, "w");
  counter = 0;

  if (out_file == NULL) {
    fprintf(stderr, "Error, couldn't open file: %s!", filename);
    return;
  }
  bhp_node* curr = n;
  while (curr->parent != NULL)
    curr = curr->parent;

  fprintf(out_file, "digraph {\n");
  bhp_node_name(curr, out_file);
  bhp_dot_node(curr, out_file);
  fprintf(out_file, "}\n");
  fclose(out_file);
}

void bhp_to_dot (bhp_heap* h, char* filename) {
  if (h->root != NULL) {
    bhp_node_to_dot(h->root, filename);
  }
}

// "Testing code"

/*
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
  } */
