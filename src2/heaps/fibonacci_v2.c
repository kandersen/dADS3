#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "fibonacci_v2.h"

void fh2_concat_list(fh2_node*, fh2_node*);
void fh2_remove_node_in_list(fh2_node*);
fh2_node* fh2_join_trees(fh2_node*,fh2_node*);
int fh2_max_rank(fh2_heap *);
void fh2_update_marked(fh2_node*,fh2_heap*);
void fh2_node_to_dot(fh2_node*, char*);

struct fh2_node {
  int key;
  int rank;
  int marked;
  item* item;
  fh2_node* parent;
  fh2_node* child;
  fh2_node* left_sibling;
  fh2_node* right_sibling;
  char* name;
};

struct fh2_heap { 
  int rank;
  fh2_node* min_node;
};

fh2_heap* fh2_make_heap() {
  fh2_heap* h =  (fh2_heap*)malloc(sizeof(fh2_heap));
  h->min_node = NULL;
  h->rank = 0;
  return h;
}

void fh2_insert_item(item* i, fh2_heap* h) {
  fh2_heap* new_h = fh2_make_heap();
  fh2_node* n = (fh2_node*)malloc(sizeof(fh2_node));
  n->key = i->key;
  n->rank = 0;
  n->marked = 0;
  n->item = i;
  n->parent = NULL;
  n->child = NULL;
  n->left_sibling = n;
  n->right_sibling = n;

  new_h->min_node = n;
  i->n = n;

  fh2_meld(h, new_h);  
}


item* fh2_find_min (fh2_heap* h) {
  if (h->min_node == NULL) {
    return NULL;
  }
  
  if (h->min_node->item != NULL) {
    return h->min_node->item;
  }

  // at this point, we have to destroy vacant nodes
  int removed_min_node = 1;
  fh2_node* current;
  while (removed_min_node == 1) {
    removed_min_node = 0;
    current = h->min_node;
    
    if (current != NULL && current->item == NULL) {
      removed_min_node = 1;
      if (current->right_sibling == current) {
        h->min_node = current->child;
      } else {
        if (current->child != NULL) {
          fh2_concat_list(current, current->child);
        }
        h->min_node = current->right_sibling;
        fh2_remove_node_in_list(current);
      }
      
      free(current);
    }
  }

  // Now, we have a fixpoint to stop at
  current = h->min_node;
  if (current != NULL) {
    do {
      fh2_node* next_current;        
      if (current->item == NULL) {
        if (current->child != NULL) {
          fh2_concat_list(current, current->child);
        }
        next_current = current->right_sibling;
        fh2_remove_node_in_list(current);
        free(current);
      } else {
        next_current = current->right_sibling;
      }
      current = next_current;          
    } while (current != h->min_node);
  
    int mr = fh2_max_rank(h);
    fh2_node* ranks[mr]; //calloc?
    for (int i = 0; i < mr; i++) {
      ranks[i] = NULL;
    }
    
    fh2_node* last_ref = h->min_node; 
          
    do {
      
      fh2_node* next_ref = last_ref->right_sibling;
      
      last_ref->parent = NULL; // remember to set parent 0 - all is root nodes
      last_ref->left_sibling = last_ref;
      last_ref->right_sibling = last_ref;
      
      fh2_node* this_ref = last_ref;            
      fh2_node* existing_tree = ranks[this_ref->rank];
      
      // a while is necessary if we join trees
      while (existing_tree != NULL) {
        ranks[this_ref->rank] = NULL;
        this_ref = fh2_join_trees(existing_tree, this_ref);
        existing_tree = ranks[this_ref->rank];
      }
      ranks[this_ref->rank] = this_ref;
      last_ref = next_ref;
      
    } while (last_ref != current); 
    
    // this way, we go through the circular list, until we reach the starting point
    fh2_node* new_min_node = NULL; // set an arbitrary one
    
    for (int i = 0; i < mr; i++) {
      fh2_node* curr_tree = ranks[i];
      if (new_min_node == NULL) {
        new_min_node = curr_tree;
      } else if (curr_tree != NULL) {
        fh2_concat_list(new_min_node, curr_tree);
        if (GT(new_min_node->key, curr_tree->key)) {
          new_min_node = curr_tree;
        }
      }
    }
    
    h->min_node = new_min_node;
  } 
  
  if (h->min_node != NULL) {
    return h->min_node->item;
  }
  
  return NULL;
}

fh2_heap* fh2_meld (fh2_heap* h1, fh2_heap* h2) {

  if (h1->min_node == NULL) { *h1 = *h2; return h1; }
  if (h2->min_node == NULL) { free(h2); return h1; }

  fh2_node* h1_min_node = h1->min_node;
  fh2_node* h2_min_node = h2->min_node;

  fh2_concat_list(h1_min_node, h2_min_node);
  
  if (h1_min_node->item == NULL) {
    h1->min_node = h1_min_node;
  } else if (h2_min_node->item == NULL) {
    h1->min_node = h2_min_node;
  } else {  
    if (GT(h1_min_node->key, h2_min_node->key)) {
      h1->min_node = h2_min_node;
    }
  }
  
  h1->rank = h1->rank + h2->rank;
  
  return h1;
}

item* fh2_delete_min (fh2_heap* h) {

  fh2_node* min_node = h->min_node;
  if (min_node != NULL) {
    min_node->item = NULL;
  }
  return NULL;
}

void fh2_decrease_key (int delta, item* i, fh2_heap* h) {

  fh2_node* n = i->n;
  
  i->key = i->key - delta;
  n->key = i->key;
  
  if (n->parent != NULL) {

    // first clean up one level up
    if (n->right_sibling == n) { // only one in the list
      n->parent->child = NULL;
      n->parent->rank = 0;
    } else {
      n->parent->child = n->right_sibling; //make sure the parents child does not point to us
      n->parent->rank = n->parent->rank - 1;
      fh2_remove_node_in_list(n);
    }

    fh2_update_marked(n->parent, h);

    n->parent = NULL;

    // move the node up to the root
    fh2_concat_list(h->min_node, n);    
  }
      
  if (GT(h->min_node->key, n->key)) {
    h->min_node = n;
  }
}

void fh2_remove_item(item* i, fh2_heap* h) {
  fh2_node* n = i->n;
  n->item = NULL;
  n->key = -1;
  h->rank = h->rank - 1;

}

int fh2_is_empty(fh2_heap* h) {
  if (h->rank > 0) {
    return 0;
  } else { 
    return 1;
  }
}


void fh2_update_marked(fh2_node* n, fh2_heap* h) {
  if (n->parent == NULL) {
    return;
  } else {
    if (n->marked == 1) {
      fh2_update_marked(n->parent, h);
      if (n->right_sibling == n) {
        n->parent->child = NULL;
      } else {
        n->parent->child = n->right_sibling;
      }
      n->parent = NULL;
      fh2_remove_node_in_list(n);
      fh2_concat_list(h->min_node, n);       
    } else {
      n->marked = 1;
    }
  }
}

void fh2_concat_list(fh2_node* n1, fh2_node* n2) {

  fh2_node* n1_curr_right = n1->right_sibling;
  fh2_node* n2_curr_left = n2->left_sibling;

  n1->right_sibling = n2;
  n2->left_sibling = n1;

  n1_curr_right->left_sibling = n2_curr_left;
  n2_curr_left->right_sibling = n1_curr_right;
}

void fh2_remove_node_in_list(fh2_node* n) {
  fh2_node* prev_sibling = n->left_sibling;
  fh2_node* next_sibling = n->right_sibling;
  
  prev_sibling->right_sibling = next_sibling;
  next_sibling->left_sibling = prev_sibling;

  n->left_sibling = n;
  n->right_sibling = n;
}

fh2_node* fh2_join_trees(fh2_node* n1, fh2_node* n2) {

  fh2_node* lowest_n;
  fh2_node* highest_n;

  if (LE(n1->key, n2->key)) {
    lowest_n = n1;
    highest_n = n2;
  } else {
    lowest_n = n2;
    highest_n = n1;
  }

  if (lowest_n->child == NULL) {
      lowest_n->child = highest_n;
      highest_n->parent = lowest_n;
  } else {
    fh2_concat_list(lowest_n->child, highest_n);
    fh2_node* temp_ref = highest_n;
    do {
      temp_ref->parent = lowest_n;
      temp_ref = temp_ref->right_sibling;
    } while (temp_ref != highest_n);
  }
  lowest_n->rank = lowest_n->rank + 1;
  highest_n->marked = 0;
  return lowest_n;
}

int fh2_max_rank(fh2_heap* h) {
  return 50 + 1; // What should be the max possible rank?
}

void
fh2_ppn(fh2_node* n) {
  printf("%s %p %p %p %p %p\n", n->name, n, n->parent, n->right_sibling, n->child, n->left_sibling);
}
int counter;
void fh2_node_name (fh2_node* n, FILE* out) {
  fh2_node* curr = n;
  char buf[10];
  
  fprintf(out, "{ rank=same;\n");
  do {
    sprintf(buf, "node%05d", ++counter);

    curr->name = (char*) malloc(sizeof(char)*10);
    strcpy(curr->name, buf);

    if (curr->item == NULL) {
      fprintf(out, "%s [label=\" \", shape=%s];\n", curr->name, curr->marked ? "box" : "oval");
    
    } else {
      fprintf(out, "%s [label=\"%d\", shape=%s];\n", curr->name, curr->key, curr->marked ? "box" : "oval");

    }
    curr = curr->left_sibling;
  }
  while (curr != n);
  fprintf(out, "}\n");

  curr = n;
  do {
    if (curr->child)
      fh2_node_name(curr->child, out);
    curr = curr->left_sibling;
  }
  while (curr != n);

}


void fh2_dot_node (fh2_node* n, FILE* out) {
  if (!n) {
    puts("called fh2_dot_node with null");
    return;
  }

  if (!(n->left_sibling)) {
    puts("n has null for left sibling!");
    return;
  }

  if (!(n->right_sibling)) {
    puts("n has null for right sibling!");
    return;
  }

  fh2_node* curr = n;
  do {
    fprintf(out, "%s -> %s\n", curr->name, curr->right_sibling->name);
    fprintf(out, "%s -> %s\n", curr->name, curr->left_sibling->name);
    if (curr->child) {
      fh2_dot_node(curr->child, out);
      fprintf(out, "%s -> %s [color=\"red\"]\n", curr->name, curr->child->name);
    }

    if (curr->parent)
      fprintf(out, "%s -> %s [color=\"blue\"]\n", curr->name, curr->parent->name);
    curr = curr->left_sibling;
  }
  while (curr != n);
}

    
void  fh2_node_to_dot (fh2_node* n, char* filename) {
  FILE * out_file = fopen(filename, "w");
  counter = 0;

  if (out_file == NULL) {
    fprintf(stderr, "Error, couldn't open file: %s!", filename);
    return;
  }

  fh2_node* curr = n;
  while (curr->parent != NULL)
    curr = curr->parent;

  fprintf(out_file, "digraph {\n");
  fh2_node_name(curr, out_file);
  fh2_dot_node(curr, out_file);
  fprintf(out_file, "}\n");
  fclose(out_file);
}

void  fh2_to_dot (fh2_heap* h, char* filename) {
  if (h->min_node != NULL) {
    fh2_node_to_dot(h->min_node, filename);
  }
}

int fh2_node_is_consistent (fh2_node* n, int parentKey) {
  
  int key = n->key;
  int item_key = n->item->key;

  if (key != item_key) {
    printf("ERROR: item key not set!!!");
    return 0;
  }

  if (key < parentKey) {
    printf("ERROR: child key smaller than parent key!!!");
    return 0;
  }

  fh2_node* parent_pointer = n->parent;
  fh2_node* sibling = n;
  fh2_node* prev_sibling = n->left_sibling;

  do{    

    if (sibling->parent != parent_pointer) {
      printf("ERROR: parent pointer not set!!!");
      return 0;
    }

    fh2_node_is_consistent(sibling, sibling->key);

    if (sibling->left_sibling != prev_sibling) {
      printf("ERROR: left sibling not set for right sibling!!!");
      return 0;
    }

    prev_sibling = sibling;
    sibling = sibling->right_sibling;

  } while (sibling != n);
  
  return 1;
}

int fh2_is_consistent(fh2_heap* h) {
  if (h->min_node != NULL) {
    return fh2_node_is_consistent(h->min_node, 0);               
  }
  return 1;
}

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
}
*/
