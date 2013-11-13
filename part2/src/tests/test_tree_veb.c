#include <time.h>
#include <stdint.h>
#include <assert.h>
#include "int_option.h"
#include "veb_only.h"


/* elements per procent */
int EPP = (1 << 24) / 100;

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

tree* create_and_fill(int fill, int* n) {
  tree * t = make_tree(24);
  permute(n, 1 << 24);

  for (int i = 0; i < fill*EPP; i++)
    insert_item(t, n[i]);

  return t;
}

void
test_insert(int fill)
{
  int* n = malloc(sizeof(int) * (1<<24));
  tree* t = create_and_fill(fill, n);

  clock_t start = clock();
  for (int i = fill*EPP; i < (1+fill)*EPP; i++)
    insert_item(t, n[i]);

  clock_t end = clock();
  printf("insert\t%d\t%d\t%ld\n", MIN_VEB_UNIVERSE_BITS, fill, end - start);
}

void
test_remove(int fill)
{
  int* n = malloc(sizeof(int) * (1<<24));
  tree* t = create_and_fill(fill, n);

  clock_t start = clock();
  for (int i = 0; i < EPP; i++)
    delete_item(t, n[i]);

  clock_t end = clock();
  printf("remove\t%d\t%ld\n", fill, end - start);
}

/* has query value */
void
test_succ1(int fill)
{
  int* n = malloc(sizeof(int) * (1<<24));
  tree* t = create_and_fill(fill, n);

  clock_t start = clock();
  for (int i = 0; i < EPP; i++)
    succ(t, n[i]);

  clock_t end = clock();
  printf("succ1\t%d\t%ld\n", fill, end - start);
}

/* might not contain */
void
test_succ2(int fill)
{
  int* n = malloc(sizeof(int) * (1<<24));
  tree* t = create_and_fill(fill, n);

  permute(n, 1 << 24);

  clock_t start = clock();
  for (int i = 0; i < EPP; i++)
    succ(t, n[i]);

  clock_t end = clock();
  printf("succ2\t%d\t%ld\n", fill, end - start);
}

void
test_contains1(int fill)
{
  int* n = malloc(sizeof(int) * (1<<24));
  tree* t = create_and_fill(fill, n);

  clock_t start = clock();
  for (int i = 0; i < EPP; i++)
    contains(t, n[i]);

  clock_t end = clock();
  printf("contains1\t%d\t%ld\n", fill, end - start);
}

void
test_contains2(int fill)
{
  int* n = malloc(sizeof(int) * (1<<24));
  tree* t = create_and_fill(fill, n);

  permute(n, 1 << 24);

  clock_t start = clock();
  for (int i = 0; i < EPP; i++)
    contains(t, n[i]);

  clock_t end = clock();
  printf("contains2\t%d\t%ld\n", fill, end - start);
}

int 

main (int c, char** v)
{
  srand(time(NULL));
  void (*f[6]) (int) = {test_insert, test_remove, test_contains1, test_contains2, test_succ1, test_succ2};
  f[atoi(v[1])](atoi(v[2]));
}
