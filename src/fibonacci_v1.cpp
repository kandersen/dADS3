#include <stdlib.h>
#include <stdio.h>
#include <cstring>
#include <unordered_map>
#include "heap.h"

typedef struct node node;
void concat_list(node*, node*);
void remove_node_in_list(node*);
node* join_trees(node*,node*);
int max_rank(heap *);
void to_dot(heap*, char*);

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
  node* min_node;
};

heap* make_heap() {
  heap* h =  (heap*)malloc(sizeof(heap));
  h->min_node = NULL;
  return h;
}

void insert(int key, void* i, heap* h) {
  heap* new_h = make_heap();

  node* n = (node*)malloc(sizeof(node));
  n->key = key;
  n->rank = 0;
  n->marked = 0;
  n->value = i;
  n->parent = NULL;
  n->child = NULL;
  n->left_sibling = n;
  n->right_sibling = n;

  new_h->min_node = n;

  *h = *meld(h, new_h);
}

void* find_min (heap* h) {
  return h->min_node->value;
}

heap* meld (heap* h1, heap* h2) {
  
  node* h1_min_node = h1->min_node;
  node* h2_min_node = h2->min_node;

  concat_list(h1_min_node, h2_min_node);

  if (h1_min_node->key > h2_min_node->key) {
    h1->min_node = h2->min_node;
  }

  return h1;
}

void* delete_min (heap* h) {

  node* min_node = h->min_node;

  node* list_to_concat = NULL;
  
  if (min_node != NULL) {

    if (min_node->left_sibling == min_node->right_sibling) { // only one root
      h->min_node = NULL;
      list_to_concat = min_node->child;
    } else { 
      // remove min root from tree
      min_node->left_sibling->right_sibling = min_node->right_sibling;
      min_node->right_sibling->left_sibling = min_node->left_sibling;

      // save an arbitrary reference to the new list
      list_to_concat = min_node->left_sibling;
      
      if (min_node->child != NULL) {
        // If we have to append childs from the min root
        concat_list(list_to_concat, min_node->child);
      }
    }
  }    
  
  if (list_to_concat != NULL) {
    
    int mr = max_rank(h);
    node* ranks[mr];
    for (int i = 0; i < mr; i++) {
      ranks[i] = NULL;
    }
    //    memset (ranks, NULL, sizeof(node)*mr);
    
    node* last_ref = list_to_concat;

    do {

      last_ref->parent = NULL; // remember to set parent 0 - all is root nodes

      node* existing_tree = ranks[last_ref->rank];

      // a while is necessary if we join trees
      while (existing_tree != NULL) {
        last_ref = join_trees(existing_tree, last_ref);
        existing_tree = ranks[last_ref->rank];
      }

      ranks[last_ref->rank] = last_ref;
      last_ref = last_ref->right_sibling;
      
    } while (last_ref != list_to_concat);

    node* new_min_node = NULL;
    for (int i = 0; i < mr; i++) {
      
      node* curr_tree = ranks[i];

      if (curr_tree != NULL) {
        if (new_min_node == NULL) {
          new_min_node = curr_tree;
        } else {
          concat_list(new_min_node, curr_tree);
          
          if (new_min_node->key > curr_tree->key) {
            new_min_node = curr_tree;
          }
        }
      }
    }

    h->min_node = new_min_node;
  }
  
  if (min_node != NULL) {
    return min_node->value;
  } else {
    return NULL;
  }
}

void decrease_key (int delta, int key, heap* h) {

  node* node;// = get_node_from_key(key);
  
  node->key = node->key - delta;
  
  if (node->parent != NULL) {

    // first clean up one level up
    if (node->right_sibling == node) { // only one in the list
      node->parent->child = NULL;
    } else {
      remove_node_in_list(node);
      node->parent->child = node->right_sibling; //make sure the parents child does not point to us
      node->parent->rank = node->parent->rank - 1;
    }

    // move the node up to the root
    concat_list(h->min_node, node);    
  }

  if (h->min_node->key > node->key) {
    h->min_node = node;
  }
}

void remove (void* i, heap* h) {
  
  node* node;// = get_node_from_key(key);
  
  if (h->min_node == node) {
    delete_min(h);
  } else {
    
    // first clean up one level up
    if (node->right_sibling == node) { // only one in the list
      node->parent->child = NULL;
    } else {
      remove_node_in_list(node);
      node->parent->child = node->right_sibling; //make sure the parents child does not point to us
      node->parent->rank = node->parent->rank - 1;
    }
    
    // move the node up to the root
    concat_list(h->min_node, node->child);    
  }
}

void  to_dot	   (heap* h);

void concat_list(node* n1, node* n2) {

  node* h1_curr_right = n1->right_sibling;
  node* h2_curr_left = n2->left_sibling;
  
  n1->right_sibling = n2;
  n2->left_sibling = n1;

  h2_curr_left->right_sibling = h1_curr_right;
  h1_curr_right->left_sibling = h2_curr_left;
}

void remove_node_in_list(node* node) {
  node->right_sibling->left_sibling = node->left_sibling->right_sibling;
  node->left_sibling->right_sibling = node->right_sibling->left_sibling;
}

node* join_trees(node* n1, node* n2) {

  if (n1->key <= n2->key) {
    concat_list(n1->child, n2);
    n1->rank = n1->rank + 1;
    return n1;
  } else {
    concat_list(n2->child, n1);
    n2->rank = n2->rank + 1;
    return n2;
  }
}

int max_rank(heap* h) {
  return 1000 + 1; // What should be the max possible rank?
}

int counter;

void dot_node (char* parent, node* child, FILE* out) {
  node* curr = child;
  char node_name[10];
  do {
    sprintf(node_name, "node%05d", ++counter);
    fprintf(out, "%s", node_name);
    fprintf(out, "%s --> %s", parent, node_name);

    if (curr->child)
      dot_node(node_name, curr->child, out);

    curr = child->left_sibling;
  }
  while (curr != child);
}


void  to_dot (heap* h, char* filename) {
  FILE * out_file = fopen(filename, "w");

  if (out_file == NULL) {
    fprintf(stderr, "Error, couldn't open file: %s!", filename);
    return;
  }

  dot_node(NULL, h->min_node->child, out_file);
  fclose(out_file);
}
