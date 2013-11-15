#include <time.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "int_option.h"
#include "search_tree.h"
#include "timer.h"

/* elements per procent */
int EPP = (1 << 24) / 100;

int *n, *m;
search_tree* t;
int fill;

void
permute(int* p, int len)
{
  int j;
  int temp;
  for (int i = 0; i < len; i++)
    p[i] = i;

  for (int i = 0; i < len; i++)
    {
      j = rand() % len;
      temp = p[i];
      p[i] = p[j];
      p[j] = temp;
    }
}

void create_and_fill(int fill) {
  t = make_search_tree();
  permute(n, 1 << 24);
  for (int i = 0; i < fill*EPP; i++) {
    insert(n[i], t);
  }
}

void
test_insert()
{
  for (int i = fill*EPP; i < (1+fill)*EPP; i++)
    insert(n[i],t);
}

void
test_remove()
{
  for (int i = 0; i < EPP; i++)
    delete_item(n[i], t);
}

/* has query value */
void
test_succ1()
{
  for (int i = 0; i < EPP; i++)
    successor_key(n[i], t);
}

/* might not contain */
void
test_succ2()
{
  for (int i = 0; i < EPP; i++)
    successor_key(m[i], t);
}

void
test_contains1()
{
  for (int i = 0; i < EPP; i++)
    search(n[i], t);
}

void
test_contains2()
{
  for (int i = 0; i < EPP; i++)
    search(m[i], t);
}

void
test_minimum()
{
  for (int i = 0; i < EPP; i++)
    minimum(t);
}

void
test_correct(int fill) {
  search_tree* t = make_search_tree();
  insert(25, t);
  insert(10, t);
  insert(18, t);
  insert(11, t);
  insert(13, t);
  insert(12, t);
  insert(14, t);
  printf("succ til 25: %d\n", successor_key(25, t));
  printf("succ til 19: %d\n", successor_key(19, t));
  printf("succ til 18: %d\n", successor_key(18, t));
  printf("succ til 17: %d\n", successor_key(17, t));
  printf("succ til 16: %d\n", successor_key(16, t));
  printf("succ til 15: %d\n", successor_key(15, t));
  printf("succ til 14: %d\n", successor_key(14, t));
  printf("succ til 13: %d\n", successor_key(13, t));
  printf("succ til 12: %d\n", successor_key(12, t));
  printf("succ til 11: %d\n", successor_key(11, t));
  printf("succ til 10: %d\n", successor_key(10, t));
  printf("succ til 9: %d\n", successor_key(9, t));
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

  void (*f[7]) (void) = {test_insert, test_remove, test_contains1, test_contains2, test_succ1, test_succ2, test_minimum};
  
  uint64_t elapsed = measure_function(f[atoi(v[1])]);

  printf("%lld\n", elapsed);
}
