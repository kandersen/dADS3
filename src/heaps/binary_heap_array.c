#include "heap.h"
#include <stdio.h>
#include <stdlib.h>

#define INITIAL_HEAP_SIZE 1024
#define INDEX_OF_ITEM(I) (I - 1)
#define ITEM_OF_INDEX(I) (I)


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

void insert_item(item* k, heap* h) {
  if(h->count == h->size) {
    h->array = (item**)realloc(h->array, (sizeof(item*) * (h->size = h->size * 2)));
  }
  h->count = h->count + 1;
  int i = h->count;
  while (i > 1) {
    int j = i / 2;
    if (GE(k->key, h->array[INDEX_OF_ITEM(j)]->key)) {
      break;
    }
    h->array[INDEX_OF_ITEM(i)] = h->array[INDEX_OF_ITEM(j)];
    h->array[INDEX_OF_ITEM(i)]->n->index = i;
    i = j;
  }
  h->array[INDEX_OF_ITEM(i)] = k;
  k->n = new_node(i);
}
    
item* find_min(heap* h) {
  if (h->count == 0) {
    return NULL;
  } else {
    return h->array[INDEX_OF_ITEM(1)];
  }
}

item* delete_min(heap* h) {

  if (h->count == 0) {
    return NULL;
  }

  item* min = (h->array[INDEX_OF_ITEM(1)]);
  item* in = h->array[INDEX_OF_ITEM(h->count)];
  h->count = h->count - 1;
  int i = 1;  
  int j;
  while ((j = 2 * i) <= h->count) {
    int right_sibling = j + 1;
    if (right_sibling > h->count) {
      right_sibling = j;
    }
    int dest;
    if (LE(h->array[INDEX_OF_ITEM(j)]->key, h->array[INDEX_OF_ITEM(right_sibling)]->key)) {
      dest = j;
    } else {
      dest = right_sibling;
    }
    item* temp = h->array[INDEX_OF_ITEM(dest)];
    if (LT(temp->key, in->key)) {
      h->array[INDEX_OF_ITEM(i)] = temp;
      temp->n->index = i;
      i = dest;
    } else {
      break;
    }
  }
  h->array[INDEX_OF_ITEM(i)] = in;
  in->n->index = i;
  //free(min->n);
  return min;
}

void decrease_key(int delta, item* k, heap* h) {
  k->key -= delta;  
  int i = k->n->index;
  while (i > 1) {
    int j = i / 2;
    if (GE(k->key, h->array[INDEX_OF_ITEM(j)]->key)) {
      break;
    }
    h->array[INDEX_OF_ITEM(i)] = h->array[INDEX_OF_ITEM(j)];
    h->array[INDEX_OF_ITEM(i)]->n->index = j;
    i = j;
  }
  h->array[INDEX_OF_ITEM(i)] = k;
  k->n->index = i;
}

int is_empty(heap* h) {
  return h->count == 0;
}

int count(heap* h) {
  return h->count;
}

// DOTTING

void item_names (heap* h, FILE* out) {
  char buf[10];
  
  fprintf(out, "{ \n");

  int i = 1;
  while (i <= h->count) {
    sprintf(buf, "node%05d", i);    
    fprintf(out, "%s [label=\"%d\"];\n", buf, h->array[i - 1]->key);
    i += 1;
  }
    
  fprintf(out, "}\n");
}


void dot_items (heap* h, FILE* out) {
  int i = 2;
  char buf1[10];
  char buf2[10];
  while (i <= h->count) {
    int j = i / 2;
    sprintf(buf1, "node%05d", i);    
    sprintf(buf2, "node%05d", j);    
    fprintf(out, "%s -> %s [color=\"red\"]\n", buf2,buf1);
    i += 1;
  }
}
    
void  to_dot (heap* h, char* filename) {
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

/*
int main() {
  heap* h = make_heap();

  char filename[50]; 
  item* items[50];
  for (int i = 49; i >= 0; i--) {
    item* t = new_item(new_foo(i), i);
    items[i] = t;
    insert(t, h);
    sprintf(filename, "file_%i.dot", 49-i);
    to_dotarr(h, filename);
  }
  
  puts("inserted");
  to_dotarr(h, "after_insert.dot");
  
  decrease_key(47, items[48], h);
  puts("Decreased key");
  
  puts("deleting");
  for (int i = 0; i < 50; i++) {
    item* min = delete_min(h);
    printf("delete_min: %i\n", min->key);
    sprintf(filename, "file_%i.dot", 50+i);
    to_dotarr(h, filename);

  }
  puts("deleted");

  return 0;
}

*/
