#ifndef SEARCH_TREE_H
#define SEARCH_TREE_H
#include "int_option.h"

struct search_tree;

typedef struct search_tree search_tree;

// CONSTRUCTOR
search_tree* make_search_tree();

// QUERIES
//int      is_empty      (search_tree*);
//int      count         (search_tree*);

int_option  minimum     (search_tree*);
int_option  maximum     (search_tree*);
int_option  successor_key   (int, search_tree*);
int_option  predecessor_key (int, search_tree*);
int_option  search      (int, search_tree*);

// MUTATORS
void insert      (int, search_tree*);
void delete      (int, search_tree*);

#endif //SEARCH_TREE_H
