#include <stdlib.h>
#include <stdio.h>
#include "heap.h"

void test_0(int);
void test_1(int);
void test_2(int);
void test_3(int);
void test_4(int);
void test_5(int);
void test_6(int);
void test_7(int);


int main(int argc, char* argv[]) {
  if (argc < 2) {
    printf("Usage: <testno> <testsize>\n");
    return 1;
  }

  switch (atoi(argv[0])) {
  case 0: 
    test_0(atoi(argv[1]));
    break;
  case 1:
    test_1(atoi(argv[1]));
    break;
  case 2:
    test_2(atoi(argv[1]));
    break;
  case 3:
    test_3(atoi(argv[1]));
    break;
  case 4:
    test_4(atoi(argv[1]));
    break;
  case 5:
    test_5(atoi(argv[1]));
    break;
  case 6:
    test_6(atoi(argv[1]));
    break;
  case 7:
    test_7(atoi(argv[1]));
    break;
  default:
    break;
  }

  return 0;
}

// pure inserts
void 
test_0 (int size) {
  heap* h = make_heap();
  for (int k = 0; k < size; k++) {
    item* i = (item*)malloc(sizeof(item));
    i->key = k;
    i->value = NULL;
    insert(i, h);
  }
}

// pure inserts 1 del
void
test_1 (int size) {
  heap* h = make_heap();
  for (int k = 0; k < size; k++) {
    item* i = (item*)malloc(sizeof(item));
    i->key = k;
    i->value = NULL;
    insert(i, h);
  }
  delete_min(h);
}

//pure inserts delmin all
void
test_2(int size) {
  heap* h = make_heap();
  for (int k = 0; k < size; k++) {
    item* i = (item*)malloc(sizeof(item));
    i->key = k;
    i->value = NULL;
    insert(i, h);
  }

  for (int kk = 0; kk < size; kk++) {
    delete_min(h);
  }
}

//mixed del inserts
void
test_3(int size) {
  heap* h = make_heap();
  for (int k = 0; k < 4*(size/10); k++) {
    item* i = (item*)malloc(sizeof(item));
    i->key = k;
    i->value = NULL;
    insert(i, h);
  }

  for (int kk = 0; kk < size/10; kk++) {
    delete_min(h);
  }

  for (int k = 0; k < 3*(size/10); k++) {
    item* i = (item*)malloc(sizeof(item));
    i->key = k;
    i->value = NULL;
    insert(i, h);
  }

  for (int kk = 0; kk < 2*(size/10); kk++) {
    delete_min(h);
  }

  for (int k = 0; k < 2*(size/10); k++) {
    item* i = (item*)malloc(sizeof(item));
    i->key = k;
    i->value = NULL;
    insert(i, h);
  }
  for (int kk = 0; kk < 3*(size/10); kk++) {
    delete_min(h);
  }
  for (int k = 0; k < size/10; k++) {
    item* i = (item*)malloc(sizeof(item));
    i->key = k;
    i->value = NULL;
    insert(i, h);
  }
  for (int kk = 0; kk < 4*(size/10); kk++) {
    delete_min(h);
  }
}

//inserts remove all (decreasing) no delmin
void
test_4(int size) {
  heap* h = make_heap();
  item* items[size];

  for (int k = 0; k < size; k++) {
    item* i = (item*)malloc(sizeof(item));
    items[k] = i;
    i->key = k;
    i->value = NULL;
    insert(items[k], h);
  }
  for (int kk = 0; kk < size; kk++) {
    remove(items[size - kk - 1], h);
  }
}

//inserts remove all (increasing) 1 delmin
void
test_5 (int size) {
  heap* h = make_heap();
  item* items[size];

  for (int k = 0; k < size; k++) {
    item* i = (item*)malloc(sizeof(item));
    items[k] = i;
    i->key = k;
    i->value = NULL;
    insert(items[k], h);
  }

  delete_min(h);

  for (int kk = 1; kk < size; kk++) {
    remove(items[kk], h);
  }  
}

//inserts remove all (decreasing) 1 delmin
void
test_6 (int size) {
  heap* h = make_heap();
  item* items[size];

  for (int k = 0; k < size; k++) {
    item* i = (item*)malloc(sizeof(item));
    items[k] = i;
    i->key = k;
    i->value = NULL;
    insert(items[k], h);
  }

  delete_min(h);

  for (int kk = 1; kk < size; kk++) {
    remove(items[size - kk - 1], h);
  }  
}

//inserts remove all (middle out) 1 delmin
void
test_7 (int size) {
  heap* h = make_heap();
  item* items[size];

  for (int k = 0; k < size; k++) {
    item* i = (item*)malloc(sizeof(item));
    items[k] = i;
    i->key = k;
    i->value = NULL;
    insert(items[k], h);
  }

  delete_min(h);

  for (int kk = 0; kk < size-1; kk++) {
    int half = size / 2;
    if (kk%2 == 0) {
      remove(items[(half+(kk/2))], h);
    } else {
      remove(items[half-((kk+1)/2)], h);
    }
  }  
}
