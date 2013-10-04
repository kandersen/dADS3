#ifndef DADS3_HEAP_H
#define DADS3_HEAP_H

#ifndef INSTRUMENT
#define LE(X, Y) ((X) <= (Y))
#define LT(X, Y) ((X) <  (Y))
#define GE(X, Y) ((X) >= (Y))
#define GT(X, Y) ((X) >  (Y))
#define EQ(X, Y) ((X) == (Y))
#else
long long cmp_counter = 0;
#define LE(X, Y) ((cmp_counter++, (X) <= (Y)))
#define LT(X, Y) ((cmp_counter++, (X) <  (Y)))
#define GE(X, Y) ((cmp_counter++, (X) >= (Y)))
#define GT(X, Y) ((cmp_counter++, (X) >  (Y)))
#define EQ(X, Y) ((cmp_counter++, (X) == (Y)))
#endif

struct heap;
struct node;
typedef struct node node;
/* IFDEF such that this is used for dijkstra and the below for dijkstra2
struct item {
  int key;
  node* n;
  void* value;
  }; */

struct item {
  int key;
  node* n;
  void* value;
  int visited;
};

typedef struct item item;
typedef struct heap heap;

heap* make_heap     ();
void  insert_item   (item* i, heap* h);
item* find_min      (heap* h);
item* delete_min    (heap* h);
heap* meld          (heap* h1, heap* h2);
void  decrease_key  (int delta, item* i, heap* h);
void  remove_item   (item* i, heap* h);
void  to_dot	    (heap* h, char* filename);
int   is_empty      (heap* h);
int   count         (heap* h);
heap* make_queue    (void* items[], int keys[], int count);
int   is_consistent (heap* h);

#endif
