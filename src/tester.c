#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include "heap.h"


void test_0(int);
void test_1(int);
void test_2(int);
void test_3(int);
void test_4(int);
void test_5(int);
void test_14(int);
void test_15(int);
void test_16(int);
void test_17(int);
void test_18(int);


int main(int argc, char* argv[]) {
  clock_t start = clock(), elapsed = 0;

  if (argc < 3) {
    printf("Usage: <testno> <testsize>\n");
    return 1;
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
  case 14:
    test_14(atoi(argv[2]));
    break;
  case 15:
    test_15(atoi(argv[2]));
    break;
  case 16:
    test_16(atoi(argv[2]));
    break;
  case 17:
    test_17(atoi(argv[2]));
    break;
  case 18:
    test_18(atoi(argv[2]));
    break;
  default:
    break;
  }

  elapsed = clock() - start;
  printf("%ld\n", elapsed);
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
    insert_item(i, h);
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
    insert_item(i, h);
  }
  delete_min(h);
}

// pure inserts 1 reverse 1 del
void
test_2 (int size) {
  heap* h = make_heap();
  for (int k = size; k >= 0; k--) {
    item* i = (item*)malloc(sizeof(item));
    i->key = k;
    i->value = NULL;
    insert_item(i, h);
  }
  delete_min(h);
}

//pure inserts delmin all
void
test_3(int size) {
  heap* h = make_heap();
  for (int k = 0; k < size; k++) {
    item* i = (item*)malloc(sizeof(item));
    i->key = k;
    i->value = NULL;
    insert_item(i, h);
  }

  for (int kk = 0; kk < size; kk++) {
    delete_min(h);
  }
}

//mixed del inserts
void
test_4(int size) {
  heap* h = make_heap();
  for (int k = 0; k < 4*(size/10); k++) {
    item* i = (item*)malloc(sizeof(item));
    i->key = k;
    i->value = NULL;
    insert_item(i, h);
  }

  for (int kk = 0; kk < size/10; kk++) {
    delete_min(h);
  }

  for (int k = 0; k < 3*(size/10); k++) {
    item* i = (item*)malloc(sizeof(item));
    i->key = k;
    i->value = NULL;
    insert_item(i, h);
  }

  for (int kk = 0; kk < 2*(size/10); kk++) {
    delete_min(h);
  }

  for (int k = 0; k < 2*(size/10); k++) {
    item* i = (item*)malloc(sizeof(item));
    i->key = k;
    i->value = NULL;
    insert_item(i, h);
  }
  for (int kk = 0; kk < 3*(size/10); kk++) {
    delete_min(h);
  }
  for (int k = 0; k < size/10; k++) {
    item* i = (item*)malloc(sizeof(item));
    i->key = k;
    i->value = NULL;
    insert_item(i, h);
  }
  for (int kk = 0; kk < 4*(size/10); kk++) {
    delete_min(h);
  }
}

// decrease key
void
test_5(int size) {
  heap* h = make_heap();
  item* items[size];
  for (int k = 0; k < size; k++) {
    item* i = (item*)malloc(sizeof(item));
    i->key = size+k;
    i->value = NULL;
    insert_item(i, h);
    items[k] = i;
  }
  for (int k = size-1; k >= 0; k--) {
    item* itm = items[k];
    decrease_key(size + 1, itm, h);
  }
}


//inserts remove all (decreasing) no delmin
void
test_14(int size) {
  heap* h = make_heap();
  item* items[size];

  for (int k = 0; k < size; k++) {
    item* i = (item*)malloc(sizeof(item));
    items[k] = i;
    i->key = k;
    i->value = NULL;
    insert_item(items[k], h);
  }
  for (int kk = 0; kk < size; kk++) {
    remove_item(items[size - kk - 1], h);
  }
}

//inserts remove all (increasing) 1 delmin
void
test_15 (int size) {
  heap* h = make_heap();
  item* items[size];

  for (int k = 0; k < size; k++) {
    item* i = (item*)malloc(sizeof(item));
    items[k] = i;
    i->key = k;
    i->value = NULL;
    insert_item(items[k], h);
  }

  delete_min(h);

  for (int kk = 1; kk < size; kk++) {
    remove_item(items[kk], h);
  }  
}

//inserts remove all (decreasing) 1 delmin
void
test_16 (int size) {
  heap* h = make_heap();
  item* items[size];

  for (int k = 0; k < size; k++) {
    item* i = (item*)malloc(sizeof(item));
    items[k] = i;
    i->key = k;
    i->value = NULL;
    insert_item(items[k], h);
  }

  delete_min(h);

  for (int kk = 1; kk < size; kk++) {
    remove_item(items[size - kk - 1], h);
  }  
}

//inserts remove all (middle out) 1 delmin
void
test_17 (int size) {
  heap* h = make_heap();
  item* items[size];

  for (int k = 0; k < size; k++) {
    item* i = (item*)malloc(sizeof(item));
    items[k] = i;
    i->key = k;
    i->value = NULL;
    insert_item(items[k], h);
  }

  delete_min(h);

  for (int kk = 0; kk < size-1; kk++) {
    int half = size / 2;
    if (kk%2 == 0) {
      remove_item(items[(half+(kk/2))], h);
    } else {
      remove_item(items[half-((kk+1)/2)], h);
    }
  }  
}

// makes a chain of nodes that are marked, performing a cascading delete
item* create_item(int key) {

  item* itm = (item*)malloc(sizeof(item));
  itm->key = key;
  itm->value = NULL;
  return itm;
}

void test_18(int size) {

  int counter = 0;
  // char fname[50];
  item* del_item;
  item* old_del_item;
  item* bottom;

  heap* h = make_heap();
  insert_item(create_item(size), h);
  bottom = create_item(size+1);
  insert_item(bottom, h);
  del_item = create_item(size+2);
  insert_item(del_item, h);
  old_del_item = create_item(size+3);
  insert_item(old_del_item, h);
  insert_item(create_item(-1), h);
  delete_min(h);

  //  sprintf(fname, "%i_1.dot", counter);
  //  to_dot(h, fname);

  remove_item(del_item, h);

  //  sprintf(fname, "%i_2.dot", counter);
  //to_dot(h, fname);

  counter += 1;

  while (counter < size) {
    
    int newroot = size - counter;

    item* itm1 = create_item(newroot);
    item* itm2 = create_item(newroot + 1);
    item* itm3 = create_item(newroot + 2);
    item* itm4  = create_item(newroot + 3);
    insert_item(itm1, h);
    insert_item(itm2, h);
    insert_item(itm3, h);
    insert_item(itm4, h);
    insert_item(create_item(-1), h);
    delete_min(h);

    //    sprintf(fname, "%i_1.dot", counter);
    //to_dot(h, fname);

    remove_item(itm2, h);
    remove_item(itm3, h);
    remove_item(old_del_item, h);

    old_del_item = itm4;
    
    //    sprintf(fname, "%i_2.dot", counter);
    //to_dot(h, fname);
    
    counter += 1;
  }

  remove_item(bottom, h);

  //  sprintf(fname, "%i.dot", counter);
  //  to_dot(h, fname);
}

