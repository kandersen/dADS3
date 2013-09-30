#include "heap.h"
#include <stdio.h>
#include <stdlib.h>

#define INITIAL_HEAP_SIZE 1024

// TYPES

struct node {
  int index;
};

struct heap {
  int  size;
  int  count;
  item** array;
};

// UTILITY

node* new_node(int i) {
  node* res = (node*) malloc(sizeof(node));
  res->index = i;
  return res;
}

// INTERFACE

heap* make_heap() {
  heap* res = (heap*) malloc(sizeof(heap));
  item** new_array = (item**) malloc(sizeof(item*) * INITIAL_HEAP_SIZE);
  res->size = INITIAL_HEAP_SIZE;
  res->count = 0;
  res->array = new_array;
  return res;
}

void insert(item* k, heap* h) {
  if(h->count == h->size) {
    h->array = (item**)realloc(h->array, (sizeof(item*) * (h-> size = h->size * 2)));
  }
  int i = h->count;
  while (i > 0) {
    int j = i / 2;
    if (GE(k->key, h->array[j]->key)) {
      break;
    }
    h->array[i] = h->array[j];
    h->array[i]->n->index = i;
    i = j;
  }
  h->array[i] = k;
  k->n = new_node(i);
  h->count = h->count + 1;
}
    
item* find_min(heap* h) {
  return h->array[0];
}

item* delete_min(heap* h) {
  item* min = h->array[0];
  h->count = h->count - 1;
  item* in = h->array[h->count];
  int i = 0;
  int j;
  while ((j = 2 * i + 1) <= h->count) {
    item* temp = h->array[j];
    item* temp1 = h->array[j + 1];
    if (LT(temp1->key, temp->key)) {
      temp = temp1;
      j = j+1;
    }
    if (GE(temp->key, in->key)) {
      break;
    }
    h->array[i] = temp;
    temp->n->index = i;
    i = j;
  }
  h->array[i] = in;
  in->n->index = i;
  free(min->n);
  return min;
}

void decrease_key(int delta, item* k, heap* h) {
  k->key -= delta;  
  int i = k->n->index;
  while (i > 0) {
    int j = i / 2;
    if (GE(k->key, h->array[j]->key)) {
      break;
    }
    h->array[i] = h->array[j];
    h->array[i]->n->index = j;
    i = j;
  }
  h->array[i] = k;
  k->n->index = i;
}

int is_empty(heap* h) {
  return h->count == 0;
}

int count(heap* h) {
  return h->count;
}
