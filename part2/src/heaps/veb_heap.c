#include <stdlib.h>
#include <stdio.h>
#include "heap.h"
#include "veb_only.h"

//#define INITIAL_HEAP_SIZE 1024;

item* make_item(int key) {
  item* res = (item*) malloc(sizeof(item));
  res->key = key;
  return res;
}

struct heap {
  tree* t;
};

heap* make_heap(uint8_t const universe) {
  heap* res = (heap*) malloc(sizeof(heap));
  res->t = make_tree(universe);
  return res;
}

void decrease_key(int delta, item* i, heap* h) {
  delete_item(h->t, i->key);
  insert_item(h->t, i->key - delta);
}

void insert_item_heap(item* i, heap* h) {
  insert_item(h->t, i->key);
}

item* find_min(heap* h) {
  item* i = make_item(minimum(h->t));
  return i;
}

item* delete_min(heap* h) {
  uint24_option min = minimum(h->t);
  if (is_some(min)) {
    delete_item(h->t, min);
    make_item(min);
  }
  return NULL;
}

heap* meld(heap* h1, heap* h2) {
  printf("NOT SUPPORTED!!!\n");
  return NULL;
}

void remove_item (item* i, heap* h) {
  delete_item(h->t, i->key);
}

int is_empty(heap* h) {
  return is_none(minimum(h->t));
}
