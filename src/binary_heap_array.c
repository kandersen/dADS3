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
    if (k->key >= h->array[j]->key) {
      break;
    }
    h->array[i] = h->array[j];
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
    if (temp1->key < temp->key) {
      temp = temp1;
      j = j+1;
    }
    if (temp->key >= in->key) {
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

int is_empty(heap* h) {
  return h->count == 0;
}

int count(heap* h) {
  return h->count;
}

// "Testing code"

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
  insert(new_item(new_foo(9), 9), h);
  insert(new_item(new_foo(4), 4), h);
  insert(new_item(new_foo(1), 1), h);
  insert(new_item(new_foo(7), 7), h);
  insert(new_item(new_foo(5), 5), h);
  insert(new_item(new_foo(5), 5), h);
  insert(new_item(new_foo(2), 2), h);
  insert(new_item(new_foo(0), 0), h);
  insert(new_item(new_foo(8), 8), h);
  insert(new_item(new_foo(3), 3), h);
  
  for(int i = 0; i < 10; i++) {
    printf("Hello, %i!\n", ((foo*)delete_min(h))->v);
  }
  return 0;
}


