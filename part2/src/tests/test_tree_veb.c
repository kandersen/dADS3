#include <time.h>
#include <stdint.h>
#include <assert.h>
#include "int_option.h"
#include "veb_only.h"

typedef struct timespec timespec;

/* elements per procent */
int EPP = (1 << 24) / 100;

timespec diff(timespec start, timespec end)
{
	timespec temp;
	if ((end.tv_nsec-start.tv_nsec)<0) {
		temp.tv_sec = end.tv_sec-start.tv_sec-1;
		temp.tv_nsec = 1000000000+end.tv_nsec-start.tv_nsec;
	} else {
		temp.tv_sec = end.tv_sec-start.tv_sec;
		temp.tv_nsec = end.tv_nsec-start.tv_nsec;
	}
	return temp;
}

void
print_result(char* test, int fill, timespec start, timespec end) {
  timespec elapsedspec = diff(start, end);
  printf("%s\t%d\t%d\t%ld\n", test, MIN_VEB_UNIVERSE_BITS, fill, elapsedspec.tv_sec * 1000000000 + elapsedspec.tv_nsec);
}

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
  for (int i = 0; i < fill*EPP; i++) {
    insert_item(t, n[i]);
  }
  return t;
}

void
test_insert(int fill)
{
  printf("%d\n", __linux);
  int* n = malloc(sizeof(int) * (1<<24));
  tree* t = create_and_fill(fill, n);

  timespec time1, time2;
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time1);

  for (int i = fill*EPP; i < (1+fill)*EPP; i++)
    insert_item(t, n[i]);

  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time2);
  print_result("insert", fill, time1, time2);
}

void
test_remove(int fill)
{
  int* n = malloc(sizeof(int) * (1<<24));
  tree* t = create_and_fill(fill, n);

  timespec time1, time2;
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time1);

  for (int i = 0; i < EPP; i++)
    delete_item(t, n[i]);

  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time2);
  print_result("remove", fill, time1, time2);
}

/* has query value */
void
test_succ1(int fill)
{
  int* n = malloc(sizeof(int) * (1<<24));
  tree* t = create_and_fill(fill, n);

  timespec time1, time2;
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time1);

  for (int i = 0; i < EPP; i++)
    succ(t, n[i]);

  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time2);
  print_result("succ1", fill, time1, time2);
}

/* might not contain */
void
test_succ2(int fill)
{
  int* n = malloc(sizeof(int) * (1<<24));
  tree* t = create_and_fill(fill, n);

  permute(n, 1 << 24);

  timespec time1, time2;
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time1);

  for (int i = 0; i < EPP; i++)
    succ(t, n[i]);

  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time2);
  print_result("succ2", fill, time1, time2);
}

void
test_contains1(int fill)
{
  int* n = malloc(sizeof(int) * (1<<24));
  tree* t = create_and_fill(fill, n);

  timespec time1, time2;
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time1);

  for (int i = 0; i < EPP; i++)
    contains(t, n[i]);

  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time2);
  print_result("contains1", fill, time1, time2);
}

void
test_contains2(int fill)
{
  int* n = malloc(sizeof(int) * (1<<24));
  tree* t = create_and_fill(fill, n);

  permute(n, 1 << 24);

  timespec time1, time2;
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time1);

  for (int i = 0; i < EPP; i++)
    contains(t, n[i]);

  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time2);
  print_result("contains2", fill, time1, time2);
}

void
test_minimum(int fill)
{
  int* n = malloc(sizeof(int) * (1<<24));
  tree* t = create_and_fill(fill, n);

  permute(n, 1 << 24);

  timespec time1, time2;
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time1);

  for (int i = 0; i < EPP; i++)
    minimum(t);

  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time2);
  print_result("minimum", fill, time1, time2);
}

int main (int c, char** v)
{
  srand(time(NULL));
  void (*f[7]) (int) = {test_insert, test_remove, test_contains1, test_contains2, test_succ1, test_succ2, test_minimum};
  f[atoi(v[1])](atoi(v[2]));
}
