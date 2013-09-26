#ifndef DADS3_HEAP_H
#define DADS3_HEAP_H

#ifndef INSTRUMENT
#define le(X, Y) ((X) <= (Y))
#define lt(X, Y) ((X) <  (Y))
#define ge(X, Y) ((X) >= (Y))
#define gt(X, Y) ((X) >  (Y))
#define eq(X, Y) ((X) == (Y))
#else
long long cmp_counter = 0;
#define le(X, Y) ((cmp_counter++, (X) <= (Y)))
#define lt(X, Y) ((cmp_counter++, (X) <  (Y)))
#define ge(X, Y) ((cmp_counter++, (X) >= (Y)))
#define gt(X, Y) ((cmp_counter++, (X) >  (Y)))
#define eq(X, Y) ((cmp_counter++, (X) == (Y)))
#endif

struct heap;
struct node;
struct item {
  int key;
  node* n;
  void* value;
};

typedef struct item item;
typedef struct heap heap;

heap* make_heap    ();
void  insert       (item* i, heap* h);
item* find_min     (heap* h);
item* delete_min   (heap* h);
heap* meld         (heap* h1, heap* h2);
void  decrease_key (int delta, item* i, heap* h);
void  remove       ( item* i, heap* h);
void  to_dot	   (heap* h, char* filename);
int   is_empty     (heap* h);
int   count        (heap* h);
heap* make_queue   (void* items[], int keys[], int count);

#endif
