#include "heap.h"
#include <stdio.h>
#include <stdlib.h>

#define INITIAL_HEAP_SIZE 1024

typedef struct pair {
  int key;
  void* value;
} pair;

pair* cons(int key, void* value) {
  pair* p = (pair*) malloc(sizeof(pair));
  p->key = key;
  p->value = value;
  return p;
}

struct heap {
  int  size;
  int  count;
  pair** array;
};

heap* make_heap() {
  heap* res = (heap*) malloc(sizeof(heap));
  pair** new_array = (pair**) malloc(sizeof(pair*) * INITIAL_HEAP_SIZE);
  res->size = INITIAL_HEAP_SIZE;
  res->count = 0;
  res->array = new_array;
  return res;
}

void insert(int key, void* item, heap* h) {
  pair* p = cons(key, item);
  if(h->count == h->size) {
    h->array = realloc(h->array, (sizeof(pair*) * (h-> size = h->size * 2)));
  }
  int i = h->count;
  while (i > 0) {
    int j = i / 2;
    if (key >= h->array[j]->key) {
      break;
    }
    h->array[i] = h->array[j];
    i = j;
  }
  h->array[i] = p;
  h->count = h->count+1;
}
    
void* find_min(heap* h) {
  return h->array[0]->value;
}

void* delete_min(heap* h) {
  void* min = h->array[0]->value;
  
  return min;
}

int is_empty(heap* h) {
  return h->count == 0;
}

int count(heap* h) {
  return h->count;
}
    

typedef struct foo {
  int v;
} foo;

foo* new_foo(int i) {
  foo* f = (foo*) malloc(sizeof(foo));
  f->v = i;
  return f;
}

int main() {
  heap* h = make_heap();
  insert(13, new_foo(78), h);
  insert(7, new_foo(42), h);
  insert(0, new_foo(7), h);
  
  printf("Hello, %i!\n", ((foo*)find_min(h))->v);
  return 0;
}


