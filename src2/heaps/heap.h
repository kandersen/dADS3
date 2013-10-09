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

struct item {
  int key;
  void* n;
  void* value;
};

typedef struct item item;

void	heap_type	(int t);
void 	make_heap	();
void 	insert_item	(item* i);
item*	find_min	();
item*	delete_min	();
void	decrease_key	(int delta, item* i);
void	remove_item	(item* i);
void	to_dot		(char* filename);
int 	is_empty	();
void    clear		();

#endif
