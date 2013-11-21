#include <time.h>
#include <stdint.h>
#include <assert.h>
#include "int_option.h"
#include "veb_tree.h"
#include "timer.h"

typedef struct vEB_tree vEB_tree;

/* elements per procent */
int EPP = (1 << 24) / 1000;

int *n, *m;
vEB_tree* t;

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
     t = vEB_init(24);
     for (int i = 0; i < fill*EPP; i++) {
          vEB_insert(t, n[i]);
     }
}

int fill;

void
test_insert()
{
     for (int i = fill*EPP; i < (1+fill)*EPP; i++)
          vEB_insert(t, n[i]);
}

void
test_remove()
{
  for (int i = 0; i < EPP; i++)
    vEB_delete(t, n[i]);
}

/* has query value */
void
test_succ1()
{
  for (int i = 0; i < EPP; i++)
    vEB_succ(t, n[i]);
}

/* might not contain */
void
test_succ2()
{
  for (int i = 0; i < EPP; i++)
    vEB_succ(t, m[i]);
}

void
test_contains1()
{
  for (int i = 0; i < EPP; i++)
    vEB_contains(t, n[i]);
}

void
test_contains2()
{
  for (int i = 0; i < EPP; i++)
    vEB_contains(t, m[i]);
}

void
test_minimum()
{
  for (int i = 0; i < EPP; i++)
    vEB_minimum(t);
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
