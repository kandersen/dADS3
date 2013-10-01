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
  if (h->min_node == NULL) {
    return NULL;
  }
  
  if (h->min_node->item != NULL) {
    return h->min_node->item;
  }

  // at this point, we have to destroy vacant nodes
  int removed_min_node = 1;
  node* current;
  while (removed_min_node == 1) {
    removed_min_node = 0;
    current = h->min_node;
    
    if (current != NULL && current->item == NULL) {
      removed_min_node = 1;
      if (current->right_sibling == current) {
        h->min_node = current->child;
      } else {
        if (current->child != NULL) {
          concat_list(current, current->child);
        }
        h->min_node = current->right_sibling;
        remove_node_in_list(current);
      }
      
      free(current);
    }
  }

  // Now, we have a fixpoint to stop at
  current = h->min_node;
  if (current != NULL) {
    do {
      node* next_current;        
      if (current->item == NULL) {
        if (current->child != NULL) {
          concat_list(current, current->child);
        }
        next_current = current->right_sibling;
        remove_node_in_list(current);
        free(current);
      } else {
        next_current = current->right_sibling;
      }
      current = next_current;          
    } while (current != h->min_node);
  
    int mr = max_rank(h);
    node* ranks[mr]; //calloc?
    for (int i = 0; i < mr; i++) {
      ranks[i] = NULL;
    }
    
    node* last_ref = h->min_node; 
          
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
      
    } while (last_ref != current); 
    
    // this way, we go through the circular list, until we reach the starting point
    node* new_min_node = NULL; // set an arbitrary one
    
    for (int i = 0; i < mr; i++) {
      node* curr_tree = ranks[i];
      if (new_min_node == NULL) {
        new_min_node = curr_tree;
      } else if (curr_tree != NULL) {
        concat_list(new_min_node, curr_tree);
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

heap* meld (heap* h1, heap* h2) {

  if (h1->min_node == NULL) { *h1 = *h2; return h1; }
  if (h2->min_node == NULL) { free(h2); return h1; }

  node* h1_min_node = h1->min_node;
  node* h2_min_node = h2->min_node;

  concat_list(h1_min_node, h2_min_node);
  
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

item* delete_min (heap* h) {

  node* min_node = h->min_node;
  if (min_node != NULL) {
    min_node->item = NULL;
  }
  return NULL;
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
      
  if (GT(h->min_node->key, n->key)) {
    h->min_node = n;
  }
}

void remove(item* i, heap* h) {
  
  node* n = i->n;
  n->item = NULL;
  n->key = -1;
  h->rank = h->rank - 1;

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

int is_consistent (node* n, int parentKey) {
  
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

  node* parent_pointer = n->parent;
  node* sibling = n;
  node* prev_sibling = n->left_sibling;

  do{    

    if (sibling->parent != parent_pointer) {
      printf("ERROR: parent pointer not set!!!");
      return 0;
    }

    is_consistent(sibling, sibling->key);

    if (sibling->left_sibling != prev_sibling) {
      printf("ERROR: left sibling not set for right sibling!!!");
      return 0;
    }

    prev_sibling = sibling;
    sibling = sibling->right_sibling;

  } while (sibling != n);
  
  return 1;
}

int is_consistent(heap* h) {
  if (h->min_node != NULL) {
    return is_consistent(h->min_node, 0);               
  }
  return 1;
}

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

  int nodes = 65;

  item* items[nodes];
  for (int i = nodes - 1; i >= 0; i--) {
    item* t = new_item(new_foo(i), i);
    items[i] = t;
    insert(t, h);
  }
  
  to_dot(h, "after_insert.dot");

  delete_min(h);
  find_min(h);

  char filename[50];
  for (int i = 0; i < 40; i++) {
    remove(items[i], h);
    sprintf(filename, "delete_%i.dot", i);
    to_dot(h, filename);
  }

  item* itm = find_min(h);
  to_dot(h, "find_min.dot");
  printf("find_min: %i\n", itm->key);

  return 0;
}


