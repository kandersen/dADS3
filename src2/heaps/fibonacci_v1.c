#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "fibonacci_v1.h"

void fh1_concat_list(fh1_node*, fh1_node*);
void fh1_remove_node_in_list(fh1_node*);
fh1_node* fh1_join_trees(fh1_node*,fh1_node*);
int fh1_max_rank(fh1_heap *);
void fh1_update_marked(fh1_node*, fh1_heap*);

struct fh1_node {
  int key;
  int rank;
  int marked;
  item* item;
  fh1_node* parent;
  fh1_node* child;
  fh1_node* left_sibling;
  fh1_node* right_sibling;
  char* name;
};

struct fh1_heap { 
  int rank;
  fh1_node* min_node;
};

fh1_heap* fh1_make_heap() {
  fh1_heap* h =  (fh1_heap*)malloc(sizeof(fh1_heap));
  h->min_node = NULL;
  h->rank = 0;
  return h;
}

void fh1_insert_item(item* i, fh1_heap* h) {
  fh1_heap* new_h = fh1_make_heap();
  fh1_node* n = (fh1_node*)malloc(sizeof(fh1_node));
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

  fh1_meld(h, new_h);  
}

item* fh1_find_min (fh1_heap* h) {
  if (h->min_node != NULL) {
    return h->min_node->item;
  }
  return NULL;
}

fh1_heap* fh1_meld (fh1_heap* h1, fh1_heap* h2) {

  if (h1->min_node == NULL) { *h1 = *h2; return h1; }
  if (h2->min_node == NULL) {return h1; }

  fh1_node* h1_min_node = h1->min_node;
  fh1_node* h2_min_node = h2->min_node;

  fh1_concat_list(h1_min_node, h2_min_node);

  if (GT(h1_min_node->key, h2_min_node->key)) {
    h1->min_node = h2->min_node;
  }

  h1->rank = h1->rank + h2->rank;
  
  free(h2);

  return h1;
}

item* fh1_delete_min(fh1_heap* h) {
  fh1_node* min_node = h->min_node;
  fh1_node* list_to_concat = NULL;

  if (min_node != NULL) {
    
    if (min_node->left_sibling == min_node) { // only one root
      h->min_node = NULL;
      list_to_concat = min_node->child;
    } else { 
      // save an arbitrary reference to the new list
      list_to_concat = min_node->left_sibling;

      // remove min root from forest
      fh1_remove_node_in_list(min_node);      
      
      if (min_node->child != NULL) {
        // If we have to append childs from the min root
        fh1_concat_list(list_to_concat, min_node->child);
      }
    }
  } 

  h->rank = h->rank - 1;
  
  if (list_to_concat != NULL) {
    
    int mr = fh1_max_rank(h);
    fh1_node* ranks[mr]; //calloc?
    for (int i = 0; i < mr; i++) {
      ranks[i] = NULL;
    }
    
    fh1_node* last_ref = list_to_concat; //which is actually just a pointer to an item
    do {

      fh1_node* next_ref = last_ref->right_sibling;
      
      last_ref->parent = NULL; // remember to set parent 0 - all is root nodes
      last_ref->left_sibling = last_ref;
      last_ref->right_sibling = last_ref;

      fh1_node* this_ref = last_ref;      

      fh1_node* existing_tree = ranks[this_ref->rank];

      // a while is necessary if we join trees
      while (existing_tree != NULL) {
        ranks[this_ref->rank] = NULL;
        this_ref = fh1_join_trees(existing_tree, this_ref);
        existing_tree = ranks[this_ref->rank];
      }
      ranks[this_ref->rank] = this_ref;
      last_ref = next_ref;
     
    } while (last_ref != list_to_concat); 

    // this way, we go through the circular list, until we reach the starting point
    fh1_node* new_min_node = NULL; // set an arbitrary one
    
    for (int i = 0; i < mr; i++) {
      fh1_node* curr_tree = ranks[i];
      if (new_min_node == NULL) {
        new_min_node = curr_tree;
      } else if (curr_tree != NULL) {
        fh1_concat_list(new_min_node, curr_tree);
        if (GT(new_min_node->key, curr_tree->key)) {
          new_min_node = curr_tree;
        }
      }
    }

    h->min_node = new_min_node;
  }

  if (min_node != NULL) {
    item* val = min_node->item;
    free(min_node);
    return val;
  } else {
    return NULL;
  }
}

void fh1_decrease_key (int delta, item* i, fh1_heap* h) {

  fh1_node* n = i->n;
  
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
      fh1_remove_node_in_list(n);
    }

    fh1_update_marked(n->parent, h);

    n->parent = NULL;

    // move the node up to the root
    fh1_concat_list(h->min_node, n);    
  }
      
  if (GT(h->min_node->key, n->key)) {
    h->min_node = n;
  }
}

void fh1_remove_item(item* i, fh1_heap* h) {
  
  fh1_node* n = i->n;

  if (h->min_node == n) {
    delete_min(h);
  } else {

    if (n->parent != NULL) {

      fh1_update_marked(n->parent, h);

      if (n->right_sibling == n) {
        n->parent->child = NULL;
      } else {
        n->parent->child = n->right_sibling;
      }

      n->parent->rank = n->parent->rank - 1;
    } 

    fh1_remove_node_in_list(n);

    if (n->child != NULL) {
      // move the node up to the root
      fh1_concat_list(h->min_node, n->child);    

      // should this be a constant operation?
      fh1_node* last_ref = n->child;
      do {
        last_ref->parent = NULL;
        last_ref = last_ref->right_sibling;
      } while (last_ref != n->child);     
    }
  } 
}

int fh1_is_empty(fh1_heap* h) {
  if (h->rank > 0) {
    return 0;
  } else { 
    return 1;
  }
}

void fh1_update_marked(fh1_node* n, fh1_heap* h) {
  if (n->parent == NULL) {
    return;
  } else {
    if (n->marked == 1) {
      fh1_update_marked(n->parent, h);
      if (n->right_sibling == n) {
        n->parent->child = NULL;
      } else {
        n->parent->child = n->right_sibling;
      }
      n->parent = NULL;
      fh1_remove_node_in_list(n);
      fh1_concat_list(h->min_node, n);       
    } else {
      n->marked = 1;
    }
  }
}

void fh1_concat_list(fh1_node* n1, fh1_node* n2) {

  fh1_node* n1_curr_right = n1->right_sibling;
  fh1_node* n2_curr_left = n2->left_sibling;

  n1->right_sibling = n2;
  n2->left_sibling = n1;

  n1_curr_right->left_sibling = n2_curr_left;
  n2_curr_left->right_sibling = n1_curr_right;
}

void fh1_remove_node_in_list(fh1_node* n) {
  fh1_node* prev_sibling = n->left_sibling;
  fh1_node* next_sibling = n->right_sibling;
  
  prev_sibling->right_sibling = next_sibling;
  next_sibling->left_sibling = prev_sibling;

  n->left_sibling = n;
  n->right_sibling = n;
}

fh1_node* fh1_join_trees(fh1_node* n1, fh1_node* n2) {

  fh1_node* lowest_n;
  fh1_node* highest_n;

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
    fh1_concat_list(lowest_n->child, highest_n);
    fh1_node* temp_ref = highest_n;
    do {
      temp_ref->parent = lowest_n;
      temp_ref = temp_ref->right_sibling;
    } while (temp_ref != highest_n);
  }
  lowest_n->rank = lowest_n->rank + 1;
  highest_n->marked = 0;
  return lowest_n;
}

int fh1_max_rank(fh1_heap* h) {
  return 50 + 1; // What should be the max possible rank?
}

void fh1_ppn(fh1_node* n) {
  printf("%s %p %p %p %p %p\n", n->name, n, n->parent, n->right_sibling, n->child, n->left_sibling);
}
int counter;
void fh1_node_name (fh1_node* n, FILE* out) {
  fh1_node* curr = n;
  char buf[10];
  
  fprintf(out, "{ rank=same;\n");
  do {
    sprintf(buf, "node%05d", ++counter);

    curr->name = (char*) malloc(sizeof(char)*10);
    strcpy(curr->name, buf);

    fprintf(out, "%s [label=\"%d\", shape=%s];\n", curr->name, curr->key, curr->marked ? "box" : "oval");
    curr = curr->left_sibling;
  }
  while (curr != n);
  fprintf(out, "}\n");

  curr = n;
  do {
    if (curr->child)
      fh1_node_name(curr->child, out);
    curr = curr->left_sibling;
  }
  while (curr != n);

}


void fh1_dot_node (fh1_node* n, FILE* out) {
  if (!n) {
    puts("called fh1_dot_node with null");
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

  fh1_node* curr = n;
  do {
    fprintf(out, "%s -> %s\n", curr->name, curr->right_sibling->name);
    fprintf(out, "%s -> %s\n", curr->name, curr->left_sibling->name);
    if (curr->child) {
      fh1_dot_node(curr->child, out);
      fprintf(out, "%s -> %s [color=\"red\"]\n", curr->name, curr->child->name);
    }

    if (curr->parent)
      fprintf(out, "%s -> %s [color=\"blue\"]\n", curr->name, curr->parent->name);
    curr = curr->left_sibling;
  }
  while (curr != n);
}

    
void  fh1_node_to_dot (fh1_node* n, char* filename) {
  FILE * out_file = fopen(filename, "w");
  counter = 0;

  if (out_file == NULL) {
    fprintf(stderr, "Error, couldn't open file: %s!", filename);
    return;
  }
  fh1_node* curr = n;
  while (curr->parent != NULL)
    curr = curr->parent;

  fprintf(out_file, "digraph {\n");
  fh1_node_name(curr, out_file);
  fh1_dot_node(curr, out_file);
  fprintf(out_file, "}\n");
  fclose(out_file);
}

void  fh1_to_dot (fh1_heap* h, char* filename) {
  if (h->min_node != NULL) {
    fh1_node_to_dot(h->min_node, filename);
  }
}

int fh1_node_is_consistent (fh1_node* n, int parentKey) {
  
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

  fh1_node* parent_pointer = n->parent;
  fh1_node* sibling = n;
  fh1_node* prev_sibling = n->left_sibling;

  do{    

    if (sibling->parent != parent_pointer) {
      printf("ERROR: parent pointer not set!!!");
      return 0;
    }

    fh1_node_is_consistent(sibling, sibling->key);

    if (sibling->left_sibling != prev_sibling) {
      printf("ERROR: left sibling not set for right sibling!!!");
      return 0;
    }

    prev_sibling = sibling;
    sibling = sibling->right_sibling;

  } while (sibling != n);
  
  return 1;
}

int fh1_is_consistent(fh1_heap* h) {
  if (h->min_node != NULL) {
    return fh1_node_is_consistent(h->min_node, 0);               
  }
  return 1;
}


