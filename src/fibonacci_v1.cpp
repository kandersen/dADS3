#include <stdlib.h>
#include <stdio.h>
#include <cstring>
#include "heap.h"

typedef struct node node;

void concat_list(node*, node*);
void remove_node_in_list(node*);
node* join_trees(node*,node*);
int max_rank(heap *);
void update_marked(node*,heap*);
void to_dot(node*, char*);

struct node {
  int key;
  int rank;
  int marked;
  item* item;
  node* parent;
  node* child;
  node* left_sibling;
  node* right_sibling;
  char* name;
};

struct heap { 
  int rank;
  node* min_node;
};

heap* make_heap() {
  heap* h =  (heap*)malloc(sizeof(heap));
  h->min_node = NULL;
  h->rank = 0;
  return h;
}

void insert(item* i, heap* h) {
  heap* new_h = make_heap();
  node* n = (node*)malloc(sizeof(node));
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

  meld(h, new_h);  
}

item* find_min (heap* h) {
  if (h->min_node != NULL) {
    return h->min_node->item;
  }
  return NULL;
}

heap* meld (heap* h1, heap* h2) {
  if (h1->min_node == NULL) { *h1 = *h2; return h1; }
  if (h2->min_node == NULL) {return h1; }

  node* h1_min_node = h1->min_node;
  node* h2_min_node = h2->min_node;

  concat_list(h1_min_node, h2_min_node);

  if (h1_min_node->key > h2_min_node->key) {
    h1->min_node = h2->min_node;
  }

  h1->rank = h1->rank + h2->rank;
  
  free(h2);

  return h1;
}

item* delete_min (heap* h) {
  node* min_node = h->min_node;
  node* list_to_concat = NULL;

  if (min_node != NULL) {
    
    if (min_node->left_sibling == min_node) { // only one root
      h->min_node = NULL;
      list_to_concat = min_node->child;
    } else { 
      // save an arbitrary reference to the new list
      list_to_concat = min_node->left_sibling;

      // remove min root from forest
      remove_node_in_list(min_node);      
      
      if (min_node->child != NULL) {
        // If we have to append childs from the min root
        concat_list(list_to_concat, min_node->child);
      }
    }
  } 

  h->rank = h->rank - 1;
  
  if (list_to_concat != NULL) {
    
    int mr = max_rank(h);
    node* ranks[mr]; //calloc?
    for (int i = 0; i < mr; i++) {
      ranks[i] = NULL;
    }
    
    node* last_ref = list_to_concat; //which is actually just a pointer to an item
    do {

      node* next_ref = last_ref->right_sibling;
      
      last_ref->parent = NULL; // remember to set parent 0 - all is root nodes
      last_ref->left_sibling = last_ref;
      last_ref->right_sibling = last_ref;

      node* this_ref = last_ref;      

      node* existing_tree = ranks[this_ref->rank];

      // a while is necessary if we join trees
      while (existing_tree != NULL) {
        ranks[this_ref->rank] = NULL;
        this_ref = join_trees(existing_tree, this_ref);
        existing_tree = ranks[this_ref->rank];
      }
      ranks[this_ref->rank] = this_ref;
      last_ref = next_ref;
     
    } while (last_ref != list_to_concat); 

    // this way, we go through the circular list, until we reach the starting point
    node* new_min_node = NULL; // set an arbitrary one
    
    for (int i = 0; i < mr; i++) {
      node* curr_tree = ranks[i];
      if (new_min_node == NULL) {
        new_min_node = curr_tree;
      } else if (curr_tree != NULL) {
        concat_list(new_min_node, curr_tree);
        if (new_min_node->key > curr_tree->key) {
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

void decrease_key (int delta, item* i, heap* h) {

  node* n = i->n;
  
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
      remove_node_in_list(n);
    }

    update_marked(n->parent, h);

    n->parent = NULL;

    // move the node up to the root
    concat_list(h->min_node, n);    
  }
      
  if (h->min_node->key > n->key) {
    h->min_node = n;
  }
}

void remove(item* i, heap* h) {
  
  node* n = i->n;

  if (h->min_node == n) {
    delete_min(h);
  } else {

    if (n->parent != NULL) {

      update_marked(n->parent, h);

      if (n->right_sibling == n) {
        n->parent->child = NULL;
      } else {
        n->parent->child = n->right_sibling;
      }

      n->parent->rank = n->parent->rank - 1;
    } 

    remove_node_in_list(n);

    if (n->child != NULL) {
      // move the node up to the root
      concat_list(h->min_node, n->child);    

      // should this be a constant operation?
      node* last_ref = n->child;
      do {
        last_ref->parent = NULL;
        last_ref = last_ref->right_sibling;
      } while (last_ref != n->child);     
    }
  } 
}

void update_marked(node* n, heap* h) {
  if (n->parent == NULL) {
    return;
  } else {
    if (n->marked == 1) {
      update_marked(n->parent, h);
      if (n->right_sibling == n) {
        n->parent->child = NULL;
      } else {
        n->parent->child = n->right_sibling;
      }
      n->parent = NULL;
      remove_node_in_list(n);
      concat_list(h->min_node, n);       
    } else {
      n->marked = 1;
    }
  }
}

void concat_list(node* n1, node* n2) {

  node* n1_curr_right = n1->right_sibling;
  node* n2_curr_left = n2->left_sibling;

  n1->right_sibling = n2;
  n2->left_sibling = n1;

  n1_curr_right->left_sibling = n2_curr_left;
  n2_curr_left->right_sibling = n1_curr_right;
}

void remove_node_in_list(node* n) {
  node* prev_sibling = n->left_sibling;
  node* next_sibling = n->right_sibling;
  
  prev_sibling->right_sibling = next_sibling;
  next_sibling->left_sibling = prev_sibling;

  n->left_sibling = n;
  n->right_sibling = n;
}

node* join_trees(node* n1, node* n2) {

  node* lowest_n;
  node* highest_n;

  if (n1->key <= n2->key) {
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
    concat_list(lowest_n->child, highest_n);
    node* temp_ref = highest_n;
    do {
      temp_ref->parent = lowest_n;
      temp_ref = temp_ref->right_sibling;
    } while (temp_ref != highest_n);
  }
  lowest_n->rank = lowest_n->rank + 1;
  highest_n->marked = 0;
  return lowest_n;
}

int max_rank(heap* h) {
  return 50 + 1; // What should be the max possible rank?
}

void
ppn(node* n) {
  printf("%s %p %p %p %p %p\n", n->name, n, n->parent, n->right_sibling, n->child, n->left_sibling);
}
int counter;
void node_name (node* n, FILE* out) {
  node* curr = n;
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
      node_name(curr->child, out);
    curr = curr->left_sibling;
  }
  while (curr != n);

}


void dot_node (node* n, FILE* out) {
  if (!n) {
    puts("called dot_node with null");
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

  node* curr = n;
  do {
    fprintf(out, "%s -> %s\n", curr->name, curr->right_sibling->name);
    fprintf(out, "%s -> %s\n", curr->name, curr->left_sibling->name);
    if (curr->child) {
      dot_node(curr->child, out);
      fprintf(out, "%s -> %s [color=\"red\"]\n", curr->name, curr->child->name);
    }

    if (curr->parent)
      fprintf(out, "%s -> %s [color=\"blue\"]\n", curr->name, curr->parent->name);
    curr = curr->left_sibling;
  }
  while (curr != n);
}

    
void  to_dot (node* n, char* filename) {
  FILE * out_file = fopen(filename, "w");
  counter = 0;

  if (out_file == NULL) {
    fprintf(stderr, "Error, couldn't open file: %s!", filename);
    return;
  }
  node* curr = n;
  while (curr->parent != NULL)
    curr = curr->parent;

  fprintf(out_file, "digraph {\n");
  node_name(curr, out_file);
  dot_node(curr, out_file);
  fprintf(out_file, "}\n");
  fclose(out_file);
}

void  to_dot (heap* h, char* filename) {
  if (h->min_node != NULL) {
    to_dot(h->min_node, filename);
  }
}

int check_consistency (node* n, int parentKey) {
  
  int key = n->key;
  int item_key = n->item->key;

  if (key != item_key) {
    printf("ERROR: item key not set!!!");
  }

  if (key < parentKey) {
    printf("ERROR: child key smaller than parent key!!!");
  }

  node* parent_pointer = n->parent;
  node* sibling = n;
  node* prev_sibling = n->left_sibling;

  do{    

    if (sibling->parent != parent_pointer) {
      printf("ERROR: parent pointer not set!!!");
    }

    check_consistency(sibling, sibling->key);

    if (sibling->left_sibling != prev_sibling) {
      printf("ERROR: left sibling not set for right sibling!!!");
    }

    prev_sibling = sibling;
    sibling = sibling->right_sibling;

  } while (sibling != n);

}


