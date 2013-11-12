#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include "heap.h"

typedef struct timespec timespec;

long long cmp_counter;

void test_0(int);
void test_1(int);
void test_2(int);
void test_3(int);
void test_4(int);
void test_5(int);
void test_6(int);

int universe;
heap* h;

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

int main(int argc, char* argv[]) {

  timespec time1, time2;

  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time1);

  if (argc < 5) {
    printf("Usage: <testno> <testsize> <universe> <construct_time> \n");
    return 1;
  }

  universe = atoi(argv[3]);
  int construct_time = atoi(argv[4]);

  if (!construct_time) {
    h = make_heap(universe);
  }

  clock_t start = clock(), elapsed = 0;

  if (construct_time) {
    h = make_heap(universe);
  }

  switch (atoi(argv[1])) {
  case 0: 
    test_0(atoi(argv[2]));
    break;
  case 1:
    test_1(atoi(argv[2]));
    break;
  case 2:
    test_2(atoi(argv[2]));
    break;
  case 3:
    test_3(atoi(argv[2]));
    break;
  case 4:
    test_4(atoi(argv[2]));
    break;
  case 5:
    test_5(atoi(argv[2]));
    break;
  case 6:
    test_6(atoi(argv[2]));
    break;
  }

  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time2);

  timespec elapsedspec = diff(time1, time2);

  printf("Seconds: %ld\n", elapsedspec.tv_sec);
  printf("nano-seconds: %ld\n", elapsedspec.tv_nsec);

  elapsed = clock() - start;
#ifndef INSTRUMENT
  printf("%ld\n", elapsed);
#else
  printf("%lld\n", cmp_counter);
#endif
  return 0;
}


// pure inserts
void 
test_0 (int size) {
  for (int k = 0; k < size; k++) {
    item* i = (item*)malloc(sizeof(item));
    i->key = k;
    insert_item(i, h);
  }
}

// pure inserts 1 del
void
test_1 (int size) {
  for (int k = 0; k < size; k++) {
    item* i = (item*)malloc(sizeof(item));
    i->key = k;
    insert_item(i, h);
  }
  delete_min(h);
}

// pure inserts 1 reverse 1 del
void
test_2 (int size) {
  for (int k = size; k >= 0; k--) {
    item* i = (item*)malloc(sizeof(item));
    i->key = k;
    insert_item(i, h);
  }
  delete_min(h);
}

//pure inserts delmin all
void
test_3(int size) {
  heap* h = make_heap(universe);
  for (int k = 0; k < size; k++) {
    item* i = (item*)malloc(sizeof(item));
    i->key = k;
    insert_item(i, h);
  }

  for (int kk = 0; kk < size; kk++) {
    delete_min(h);
  }
}

//mixed del inserts
void
test_4(int size) {
  for (int k = 0; k < 4*(size/10); k++) {
    item* i = (item*)malloc(sizeof(item));
    i->key = k;
    insert_item(i, h);
  }

  for (int kk = 0; kk < size/10; kk++) {
    delete_min(h);
  }

  for (int k = 0; k < 3*(size/10); k++) {
    item* i = (item*)malloc(sizeof(item));
    i->key = k;
    insert_item(i, h);
  }

  for (int kk = 0; kk < 2*(size/10); kk++) {
    delete_min(h);
  }

  for (int k = 0; k < 2*(size/10); k++) {
    item* i = (item*)malloc(sizeof(item));
    i->key = k;
    insert_item(i, h);
  }
  for (int kk = 0; kk < 3*(size/10); kk++) {
    delete_min(h);
  }
  for (int k = 0; k < size/10; k++) {
    item* i = (item*)malloc(sizeof(item));
    i->key = k;
    insert_item(i, h);
  }
  for (int kk = 0; kk < 4*(size/10); kk++) {
    delete_min(h);
  }
}

// decrease key
void
test_5(int size) {
  int target = size / 2 - 1;
  item* items[target];
  for (int k = 0; k < target; k++) {
    item* i = (item*)malloc(sizeof(item));
    i->key = k + target;
    insert_item(i, h);
    i->key = k + target; // vEB resets the key
    items[k] = i;
  }
  for (int k = target-1; k >= 0; k--) {
    item* itm = items[k];
    decrease_key(target + 1, itm, h);
  }
}

// keys distributed across the entire universe
void test_6(int size) {
  int padding = 0;
  if (2 * size < universe) {
    padding = universe / size;
  }
  for (int i = 0; i < universe - 1; i++) {
    item* itm = (item*)malloc(sizeof(item));
    itm->key = i;
    insert_item(itm, h);
    i += padding;
  } 
  item* min = delete_min(h);
  while (min) {    
    min = delete_min(h);
  }
}
