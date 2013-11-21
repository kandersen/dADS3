#include <stdlib.h>
#include <stdio.h>
#include "rb_tree.h"
#include "heap.h"

#define INITIAL_HEAP_SIZE 1024;

struct node {
  rb_node* rb;
};

item* make_item(int key) {
  item* res = (item*) malloc(sizeof(item));
  res->key = key;
  return res;
}

struct heap {
  rb_tree* t;
};

heap* make_heap(uint8_t universe) {
  heap* res = (heap*) malloc(sizeof(heap));
  res->t = make_rb_tree();
  return res;
}

void decrease_key(int delta, item* i, heap* h) {
  rb_delete(h->t, i->n->rb);
  rb_node* rbn = i->n->rb;
  rbn->key = rbn->key - delta;
  rbn->color = 0;
  rbn->right = NULL;
  rbn->left = NULL;
  rbn->p = NULL;
  rb_insert(h->t, rbn);
}

void insert_item_heap(item* i, heap* h) {
  rb_node* rbnode = make_rb_node(i->key);
  node* nx = (node*)malloc(sizeof(node));
  rbnode->i = i;
  nx->rb = rbnode;
  i->n = nx;
  rb_insert(h->t, rbnode);
}

item* find_min(heap* h) {
  rb_node* res = rb_minimum(h->t, rb_root_of(h->t));
  if(res) {
    return make_item(rb_key_of(res));
  } else {
    return make_item(none());
  }
}

item* delete_min(heap* h) {
  rb_node* root = rb_root_of(h->t);
  if (root) {
    rb_node* res = rb_minimum(h->t, root);
    if(res) {
      rb_delete(h->t, res);
      res->i->n = NULL;
      return make_item(rb_key_of(res));
    } else {
      return NULL;
    }
  } else {
    return NULL;
  }
}

/*
heap* meld(heap* h1, heap* h2) {
  item* next = NULL;
  while(is_some((next = delete_min(h2))->key)) {
    insert_item(next, h1);
  }
  return h1;
  } */

void remove_item (item* i, heap* h) {
  if (i->n != NULL) 
    rb_delete(h->t, i->n->rb);
  i->n = NULL;
}

void to_dot (heap* h, char* filename) {
  puts("rb_heap.c:to_dot not implemented yet! -- Crashing the program");
  printf("Crashing program! %i", 4 / count(make_heap(0)));
}

int is_empty(heap* h) {
  return rb_is_empty(h->t);
}

int count(heap* h) {
  return rb_count(h->t);
}

/*
heap* make_queue (void* items[], int keys[], int count) {
  heap* res = make_heap(0);
  for(int i = 0; i < count; i++) {
    insert_item(make_item(keys[i]), res);
  }
  return res;
  } */

int is_consistent (heap* h) {
  puts("rb_heap.c:is_consistent not implemented yet! -- Crashing the program");
  printf("Crashing program! %i", 4 / count(make_heap(0)));
  return 0;
}
