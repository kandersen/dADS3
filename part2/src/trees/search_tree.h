#ifndef RB_TREE_H
#define RB_TREE_H

struct search_tree;

typedef struct search_tree search_tree;

// CONSTRUCTOR
search_tree* make_search_tree();

// QUERIES
//int      is_empty      (search_tree*);
//int      count         (search_tree*);

int  minimum     (search_tree*);
int  maximum     (search_tree*);
int  successor   (search_tree*);
int  predecessor (search_tree*);
int  search      (int, search_tree*);

// MUTATORS
void insert      (int, search_tree*);
void delete      (int, search_tree*);

#endif //RB_TREE_H
