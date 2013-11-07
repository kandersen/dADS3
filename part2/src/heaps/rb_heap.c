#include <stdlib.h>
#include <stdio.h>
#include "rb_tree.h"
#include "heaps/heap.h"

#define INITIAL_HEAP_SIZE 1024;

/*
  struct item {
    int_option key;
    node* n;
  };
*/

item* make_item(int key) {
  item* res = (item*) malloc(sizeof(item));
  res->key = key;
  return res;
}

struct node { };

struct heap {
  rb_tree* t;
};

heap* make_heap() {
  heap* res = (heap*) malloc(sizeof(heap));
  res->t = make_rb_tree();
  return res;
}

void insert_item(item* i, heap* h) {
  rb_insert(h->t, make_rb_node(i->key));
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
  rb_node* res = rb_minimum(h->t, rb_root_of(h->t));
  if(res) {
    rb_delete(h->t, res);
    return make_item(rb_key_of(res));
  } else {
    return make_item(none());
  }
}

heap* meld(heap* h1, heap* h2) {
  item* next = NULL;
  while(is_some((next = delete_min(h2))->key)) {
    insert_item(next, h1);
  }
  return h1;
}

void remove_item (item* i, heap* h) {
  rb_delete(h->t, make_rb_node(i->key));
}

void to_dot (heap* h, char* filename) {
  puts("rb_heap.c:to_dot not implemented yet! -- Crashing the program");
  printf("Crashing program! %i", 4 / count(make_heap()));
}

int is_empty(heap* h) {
  return rb_is_empty(h->t);
}

int count(heap* h) {
  return rb_count(h->t);
}

heap* make_queue (void* items[], int keys[], int count) {
  heap* res = make_heap();
  for(int i = 0; i < count; i++) {
    insert_item(make_item(keys[i]), res);
  }
  return res;
}

int is_consistent (heap* h) {
  puts("rb_heap.c:is_consistent not implemented yet! -- Crashing the program");
  printf("Crashing program! %i", 4 / count(make_heap()));
}
