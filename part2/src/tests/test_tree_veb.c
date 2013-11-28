#include <time.h>
#include <stdint.h>
#include <assert.h>
#include "int_option.h"
#include "veb_only.h"
#include "timer.h"

/* elements per procent */
int EPP = (1 << 24) / 1000;

int *n, *m;
tree* t;
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
  t = make_tree(24);
  for (int i = 0; i < fill*EPP; i++) {
    insert_item(t, n[i]);
  }
}

void
test_insert()
{
  for (int i = fill*EPP; i < (1+fill)*EPP; i++)
    insert_item(t, n[i]);
}

void
test_remove()
{
  for (int i = 0; i < EPP; i++)
    delete_item(t, n[i]);
}

/* has query value */
void
test_succ1()
{
  for (int i = 0; i < EPP; i++) {
    if (n[i] > 0) {
      succ(t, n[i]-1);
    } else {
      succ(t, n[i]);
    }
  }
}

/* might not contain */
void
test_succ2()
{
  for (int i = 0; i < EPP; i++)
    succ(t, m[i]);
}

void
test_contains1()
{
  for (int i = 0; i < EPP; i++)
    contains(t, n[i]);
}

void
test_contains2()
{
  for (int i = 0; i < EPP; i++)
    contains(t, m[i]);
}

void
test_minimum()
{
  for (int i = 0; i < EPP; i++)
    minimum(t);
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
