#include <time.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include "int_option.h"
#include "heap.h"
#include "timer.h"

/* elements per promille */
int EPP = (1 << 24) / 1000;

int *n, *m, *dec;
heap* t;
int fill; 
item** items;

void
permute(int* p, int len)
{
  int j;
  int temp;
  for (int i = 1; i < len; i++)
    p[i-1] = i;

  for (int i = 0; i < len; i++)
    {
      j = rand() % len;
      if (j > 0) {
        temp = p[i];
        p[i] = p[j];
        p[j] = temp;
      }
    }
}

item* create_item(int key) {
  item* itm = (item*)malloc(sizeof(item));
  itm->key = key;
  return itm;
}

void create_and_fill(int fill) {
  t = make_heap(24);
  for (int i = 0; i < (fill-1)*EPP; i++) {
    insert_item_heap(create_item(n[i]), t);
  }
}

void
test_insert()
{
  for (int i = fill*EPP; i < (1+fill)*EPP; i++) {
    insert_item_heap(create_item(n[i]),t);
  }

  delete_min(t);
}

void
test_remove()
{
  for (int i = 0; i < EPP; i++)
    remove_item(items[i], t);

  delete_min(t);
}

void
test_delete_min()
{
  for (int i = 0; i < EPP; i++)
    delete_min(t);
}

void
test_decrease_key()
{
  for (int i = 0; i < EPP; i++)
    decrease_key(dec[i], items[i], t);

  delete_min(t);
}

void
test_minimum()
{
  for (int i = 0; i < EPP; i++)
    find_min(t);
}

int main (int c, char** v)
{
  srand(time(NULL));
  n = malloc(sizeof(int) * (1<<24));
  m = malloc(sizeof(int) * (1<<24));
  permute(n, 1 << 24); 
  permute(m, 1 << 24);
  fill = atoi(v[2]);

  create_and_fill(fill);
  item* null_key_item = create_item(0);
  insert_item_heap(null_key_item, t);

  int start = (fill-1)*EPP;
  items = (item**)malloc(sizeof(item*) * EPP);
  for (int i = start; i < (fill)*EPP; i++) {
    item* itm = create_item(n[i]);
    items[i-start] = itm;
    insert_item_heap(itm, t); 
 }
  delete_min(t);
  find_min(t);
  // generate decrease keys before timing
  dec = malloc(sizeof(int) * EPP);
  for (int i = 0; i < EPP; i++) {
    if (items[i]->key <= 0) {
      dec[i] = 0;
    } else {
      dec[i] = (int)(rand() % items[i]->key);
    }
  }

  void (*f[5]) (void) = {test_insert, test_remove, test_delete_min, test_decrease_key, test_minimum};
  
  uint64_t elapsed = measure_function(f[atoi(v[1])]);

  printf("%lld\n", elapsed);
}
