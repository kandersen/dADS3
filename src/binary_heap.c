#include "heap.h"
#include <stdlib.h>
#include "heap.h"

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

int is_empty(heap* h) {
  return h->count == 0;
}

int count(heap* h) {
  return h->count;
}
    


