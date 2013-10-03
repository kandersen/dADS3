#include "heap.h"
#include <stdio.h>
#include <stdlib.h>

#define INITIAL_HEAP_SIZE 1024
#define INDEX_OF_ITEM(I) (I - 1)
#define ITEM_OF_INDEX(I) (I + 1)


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
    h->array[INDEX_OF_ITEM(i)]->n->index = INDEX_OF_ITEM(i);
    i = j;
  }
  h->array[INDEX_OF_ITEM(i)] = k;
  k->n = new_node(INDEX_OF_ITEM(i));

}
    
item* find_min(heap* h) {
  return h->array[INDEX_OF_ITEM(1)];
}

item* delete_min(heap* h) {
  printf("count %i", h->count);
  puts("1");
  if (h->count == 0) {
    return NULL;
  }

  puts("2");
  item* min = h->array[INDEX_OF_ITEM(1)];
  puts("3");
  item* in = h->array[INDEX_OF_ITEM(h->count)];
  puts("4");
  h->count = h->count - 1;
  puts("5");
  int i = 1;
  int j;
  puts("6");
  while ((j = 2 * i) <= h->count) {
    puts("7");
    item* temp = h->array[INDEX_OF_ITEM(j)];
    puts("8");
    item* temp1 = h->array[INDEX_OF_ITEM(j + 1)];
    puts("9");
    if (LT(temp1->key, temp->key)) {
      puts("10");
      temp = temp1;
      puts("11");
      j = j+1;
    }
    puts("12");
    if (GE(temp->key, in->key)) {
      puts("13");      
      break;
    }
    puts("14");     
    h->array[INDEX_OF_ITEM(i)] = temp;
    puts("15");
    temp->n->index = i;
    puts("16");
    i = j;
  }
  puts("17");
  h->array[INDEX_OF_ITEM(i)] = in;
  puts("18");
  in->n->index = i;
  puts("19");
  //free(min->n);
  puts("20");
  return min;
}

void decrease_key(int delta, item* k, heap* h) {
  k->key -= delta;  
  int i = ITEM_OF_INDEX(k->n->index);
  while (i > 1) {
    int j = i / 2;
    if (GE(k->key, h->array[INDEX_OF_ITEM(j)]->key)) {
      break;
    }
    h->array[INDEX_OF_ITEM(i)] = h->array[INDEX_OF_ITEM(j)];
    h->array[INDEX_OF_ITEM(i)]->n->index = INDEX_OF_ITEM(j);
    i = j;
  }
  h->array[INDEX_OF_ITEM(i)] = k;
  k->n->index = INDEX_OF_ITEM(i);
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
