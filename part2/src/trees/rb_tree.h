#ifndef RB_TREE_H
#define RB_TREE_H

typedef int color;

struct rb_node;
struct rb_tree;

typedef struct rb_node rb_node;
typedef struct rb_tree rb_tree;

// CONSTRUCTORS
rb_tree* make_rb_tree   ();
rb_node* make_rb_node   (int);

// QUERIES
int      rb_key_of      (rb_node*);

rb_node* rb_root_of     (rb_tree*);
int      rb_is_empty    (rb_tree*);
int      rb_count       (rb_tree*);

rb_node* rb_minimum     (rb_tree*, rb_node*);
rb_node* rb_maximum     (rb_tree*, rb_node*);
rb_node* rb_successor   (rb_tree*, rb_node*);
int rb_successor_key   (rb_tree*, int);
rb_node* rb_predecessor (rb_tree*, rb_node*);
int rb_predecessor_key   (rb_tree*, int);
rb_node* rb_search      (rb_tree*, int);

// MUTATORS
void     rb_insert      (rb_tree*, rb_node*);
void     rb_delete      (rb_tree*, rb_node*);

#endif //RB_TREE_H
