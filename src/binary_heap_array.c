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
<<<<<<< HEAD

// DOTTING

void item_names (heap* h, FILE* out) {
  char buf[10];
  
  fprintf(out, "{ \n");

  int i = 0;
  while (i < h->count) {
    sprintf(buf, "node%05d", i);    
    fprintf(out, "%s [label=\"%d\"];\n", buf, h->array[i]->key);
    i += 1;
  }
    
  fprintf(out, "}\n");
}


void dot_items (heap* h, FILE* out) {
  int i = 0;
  char buf1[10];
  char buf2[10];
  while (i < h->count) {
    if (i > 0) {
      int j = i / 2;
      sprintf(buf1, "node%05d", i);    
      sprintf(buf2, "node%05d", j);    
      fprintf(out, "%s -> %s [color=\"red\"]\n", buf2,buf1);
    } 
    i += 1;
  }
}
    
void  to_dotarr (heap* h, char* filename) {
  FILE * out_file = fopen(filename, "w");

  if (out_file == NULL) {
    fprintf(stderr, "Error, couldn't open file: %s!", filename);
    return;
  }

  fprintf(out_file, "digraph {\n");
  item_names(h, out_file);
  dot_items(h, out_file);
  fprintf(out_file, "}\n");
  fclose(out_file);
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
 
  item* items[50];
  for (int i = 49; i >= 0; i--) {
    item* t = new_item(new_foo(i), i);
    items[i] = t;
    insert(t, h);
  }
  
  puts("inserted");
  to_dotarr(h, "after_insert.dot");
  
  puts("deleting");
  char filename[50];
  for (int i = 0; i < 50; i++) {
    item* min = delete_min(h);
    sprintf(filename, "file_%i.dot", i);
    to_dotarr(h, filename);
    printf("delete_min: %i\n", min->key);
  }
  puts("deleted");

  return 0;
}


=======
>>>>>>> 74db49f479dd6d99975c9d1e6fde9e5bbe02edf2
