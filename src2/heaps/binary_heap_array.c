#include <stdio.h>
#include <stdlib.h>
#include "binary_heap_array.h"

#define INITIAL_HEAP_SIZE 1024
#define INDEX_OF_ITEM(I) (I - 1)
#define ITEM_OF_INDEX(I) (I)


// TYPES

struct bha_node {
  int index;
};

struct bha_heap {
  int  size;
  int  count;
  item** array;
};

// UTILITY

bha_node* bha_new_node(int i) {
  bha_node* res = (bha_node*) malloc(sizeof(bha_node));
  res->index = i;
  return res;
}

// INTERFACE

bha_heap* bha_make_heap() {
  bha_heap* res = (bha_heap*) malloc(sizeof(bha_heap));
  item** new_array = (item**) malloc(sizeof(item*) * INITIAL_HEAP_SIZE);
  res->size = INITIAL_HEAP_SIZE;
  res->count = 0;
  res->array = new_array;
  return res;
}

void bha_insert_item(item* k, bha_heap* h) {
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
    bha_node* n = h->array[INDEX_OF_ITEM(i)]->n;
    n->index = i;
    i = j;
  }
  h->array[INDEX_OF_ITEM(i)] = k;
  k->n = bha_new_node(INDEX_OF_ITEM(i));
}
    
item* bha_find_min(bha_heap* h) {
  if (h->count == 0) {
    return NULL;
  } else {
    return h->array[INDEX_OF_ITEM(1)];
  }
}

item* bha_delete_min(bha_heap* h) {

  if (h->count == 0) {
    return NULL;
  }

  item* min = (h->array[INDEX_OF_ITEM(1)]);
  item* in = h->array[INDEX_OF_ITEM(h->count)];
  h->count = h->count - 1;
  int i = 1;  
  int j;
  while ((j = 2 * i) <= h->count) {
    item* temp = h->array[INDEX_OF_ITEM(j)];
    if (LT(temp->key, in->key)) {
      h->array[INDEX_OF_ITEM(i)] = temp;
      bha_node* n = temp->n;
      n->index = i;
      i = j;
    } else {
      break;
    }
  }
  h->array[INDEX_OF_ITEM(i)] = in;
  bha_node* tmp2 = in->n;
  tmp2->index = i;
  //free(min->n);
  return min;
}

void bha_remove_item(item* i, bha_heap* h) {

}

void bha_decrease_key(int delta, item* k, bha_heap* h) {
  k->key -= delta;  
  bha_node* tmp = k->n;
  int i = ITEM_OF_INDEX(tmp->index);
  while (i > 1) {
    int j = i / 2;
    if (GE(k->key, h->array[INDEX_OF_ITEM(j)]->key)) {
      break;
    }
    h->array[INDEX_OF_ITEM(i)] = h->array[INDEX_OF_ITEM(j)];
    tmp = h->array[INDEX_OF_ITEM(i)]->n;
    tmp->index = j;
    i = j;
  }
  h->array[INDEX_OF_ITEM(i)] = k;
  tmp->index = i;
}

int bha_is_empty(bha_heap* h) {
  return h->count == 0;
}

int bha_count(bha_heap* h) {
  return h->count;
}

// DOTTING

void bha_item_names (bha_heap* h, FILE* out) {
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


void bha_dot_items (bha_heap* h, FILE* out) {
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
    
void  bha_to_dot (bha_heap* h, char* filename) {
  FILE * out_file = fopen(filename, "w");

  if (out_file == NULL) {
    fprintf(stderr, "Error, couldn't open file: %s!", filename);
    return;
  }
  
  fprintf(out_file, "digraph {\n");
  bha_item_names(h, out_file);
  bha_dot_items(h, out_file);
  fprintf(out_file, "}\n");
  fclose(out_file);
}

void bha_clear(bha_heap* h) {
  free(h->array);
}

// "Testing code"
/*
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
  bha_heap* h = bha_make_heap();

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
