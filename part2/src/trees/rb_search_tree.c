#include <stdlib.h>
#include "rb_tree.h"
#include "search_tree.h"
#include "int_option.h"

struct search_tree {
  rb_tree* t;
};

// CONSTRUCTOR
search_tree* make_search_tree() {
  search_tree* res = (search_tree*) malloc(sizeof(search_tree));
  res->t = make_rb_tree();
  return res;
}

// QUERIES
int_option minimum (search_tree* st) {
  rb_node* res = rb_minimum(st->t, rb_root_of(st->t));
  if(res) {
    return some(rb_key_of(res));
  } else {
    return none();
  }
}

int_option maximum (search_tree* st) {
  rb_node* res = rb_maximum(st->t, rb_root_of(st->t));
  if(res) {
    return some(rb_key_of(res));
  } else {
    return none();
  }
}

int_option successor_key (int key, search_tree* st) {
  int res = rb_successor_key(st->t, key);
  return some(res);
}

int_option predecessor_key (int key, search_tree* st) {
  int res = rb_predecessor_key(st->t, key);
  return some(res);
}

int_option search (int key, search_tree* st) {
  rb_node* res = rb_search(st->t, key);
  if(res) {
    return some(rb_key_of(res));
  } else {
    return none();
  }
}

// MUTATORS
void insert (int key, search_tree* st) {
  rb_insert(st->t, make_rb_node(key));
}

void delete_item (int key, search_tree* st) {
  rb_node* target = rb_search(st->t, key);
  if (target) rb_delete(st->t, target);
}


