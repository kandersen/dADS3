#define BLACK 0
#define RED 1

#include <stdlib.h>
#include "rb_tree.h"

// NODE TYPE, CLRS pp. 309

struct rb_node {
  color color;
  int key; 
  rb_node* right; 
  rb_node* left;
  rb_node* p;
};

int rb_key_of(rb_node* n) {
  return n->key;
}

rb_node* make_rb_node(int key) {
  rb_node* res = (rb_node*) malloc(sizeof(rb_node));
  res->color = BLACK;
  res->key = key;
  res->right = NULL;
  res->left = NULL;
  res->p = NULL;
  return res;
}

rb_node* make_sentinel() {
  return make_rb_node(0);
}

// TREE TYPE

struct rb_tree {
  rb_node* nil;
  rb_node* root;
  int count;
};

rb_node* rb_root_of(rb_tree* t) {
  return t->root;
}

int rb_count(rb_tree* t) {
  return t->count;
}

int rb_is_empty(rb_tree* t) {
  return t->count == 0;
}

rb_tree* make_rb_tree() {
  rb_tree* res = (rb_tree*) malloc(sizeof(rb_tree));
  res->nil = make_sentinel();
  res->root = res->nil;
  res->count = 0;
  return res;
}

// PRIVATE UTILITIES

void left_rotate(rb_tree* T, rb_node* x) {
  rb_node* y = x->right;
  x->right = y->left;
  if (y->left != T->nil)
    y->left->p = x;
  y->p = x->p;
  if (x->p == T->nil) {
    T->root = y;
  } else if (x == x->p->left) {
    x->p->left = y;
  } else {
    x->p->right = y;
  }
  y->left = x;
  x->p = y;
}

void right_rotate(rb_tree* T, rb_node* y) {
  rb_node* x = y->left;
  y->left = x->right;
  if(x->right != T->nil)
    x->right->p = y;
  x->p = y->p;
  if (y->p == T->nil) {
    T->root = x;
  } else if (y == y->p->left) {
    y->p->left = x;
  } else {
    y->p->right = x;
  }
  x->right = y;
  y->p = x;
}

void insert_fixup(rb_tree* T, rb_node* z) {
  while (z->p->color == RED) {
    if (z->p == z->p->p->left) {
      rb_node* y = z->p->p->right;
      if (y->color == RED) {
	z->p->color = BLACK;
	y->color = BLACK;
	z->p->p->color = RED;
	z = z->p->p;
      } else {
	if (z == z->p->right) {
	  z = z->p;
	  left_rotate(T, z);
	}
	z->p->color = BLACK;
	z->p->p->color = RED;
	right_rotate(T, z->p->p);
      }
    } else {
      if (z->p == z->p->p->right) {
	rb_node* y = z->p->p->left;
	if (y->color == RED) {
	  z->p->color = BLACK;
	  y->color = BLACK;
	  z->p->p->color = RED;
	  z = z->p->p;
	} else {
	  if (z == z->p->left) {
	    z = z->p;
	    right_rotate(T, z);
	  }
	  z->p->color = BLACK;
	  z->p->p->color = RED;
	  left_rotate(T, z->p->p);
	}
      }
    }
  }
  T->root->color = BLACK;
}

void transplant(rb_tree* T, rb_node* u, rb_node* v) {
  if (u->p == T->nil) {
    T->root = v;
  } else if (u == u->p->left) {
    u->p->left = v;
  } else {
    u->p->right = v;
  }
  v->p = u->p;
}

void delete_fixup(rb_tree* T, rb_node* x) {
  while (x != T->root && x->color == BLACK) {
    if (x == x->p->left) {
      rb_node* w = x->p->right;
      if (w->color == RED)  {
	w->color = BLACK;
	x->p->color = RED;
	left_rotate(T, x->p);
	w = x->p->right;
      }
      if (w->left->color == BLACK && w->right->color == BLACK) {
	w->color = RED;
	x = x->p;
      } else {
	if (w->right->color == BLACK) {
	  w->left->color = BLACK;
	  w->color = RED;
	  right_rotate(T, w);
	  w = x->p->right;
	}
	w->color = x->p->color;
	x->p->color = BLACK;
	w->right->color = BLACK;
	left_rotate(T, x->p);
	x = T->root;
      }
    } else {
      rb_node* w = x->p->left;
      if (w->color == RED)  {
	w->color = BLACK;
	x->p->color = RED;
	right_rotate(T, x->p);
	w = x->p->left;
      }
      if (w->right->color == BLACK && w->left->color == BLACK) {
	w->color = RED;
	x = x->p;
      } else {
	if (w->left->color == BLACK) {
	  w->right->color = BLACK;
	  w->color = RED;
	  left_rotate(T, w);
	  w = x->p->left;
	}
	w->color = x->p->color;
	x->p->color = BLACK;
	w->left->color = BLACK;
	right_rotate(T, x->p);
	x = T->root;
      }
    } 
  }
  x->color = BLACK;
}

//QUERIES

rb_node* rb_minimum(rb_tree* T, rb_node* x) {
  while (x->left != T->nil) {
    x = x->left;
  }
  return x;
}   

rb_node* rb_maximum(rb_tree* T, rb_node* x) {
  while (x->right != T->nil) {
    x = x->right;
  }
  return x;
}

rb_node* rb_successor(rb_tree* T, rb_node* x) {
  if (x->right != T->nil) {
    return rb_minimum(T, x->right);
  }
  rb_node* y = x->p;
  while (y != T->nil && x == y->right) {
    x = y;
    y = y->p;
  }
  return y;
}

rb_node* rb_predecessor(rb_tree* T, rb_node* x) {
  if (x->left != T->nil) {
    return rb_maximum(T, x->left);
  }
  rb_node* y = x->p;
  while(y != T->nil && x == y->left) {
    x = y;
    y = y->p;
  } 
  return y;
}

rb_node* rb_search(rb_tree* T, int k) {
  rb_node* x = T->root;
  while (x != T->nil && k == x->key) {
    if (k < x->key) {
      x = x->left;
    } else {
      x = x->right;
    }
  }
  return x;
}
//MUTATORS

void rb_insert (rb_tree* T, rb_node* z) {
  rb_node* y = T->nil;
  rb_node* x = T->root;
  while (x != T->nil) {
    y = x;
    if (z->key < x->key) {
      x = x->left;
    } else {
      x = x->right;
    }
  }
  z->p = y;
  if (y == T->nil) {
    T->root = z;
  } else if (z->key < y->key) {
    y->left = z;
  } else {
    y->right = z;
  }
  z->left = T->nil;
  z->right = T->nil;
  z->color = RED;
  insert_fixup(T, z);
  T->count++;
}
  
void rb_delete(rb_tree* T, rb_node* z) {
  rb_node* y = z;
  color y_original_color = y->color;
  rb_node* x = NULL;
  if (z->left == T->nil) {
    x = z->right;
    transplant(T, z, z->right);
  } else if (z->right == T->nil) {
    x = z->left;
    transplant(T, z, z->left);
  } else {
    y = rb_minimum(T, z->right);
    y_original_color = y->color;
    x = y->right;
    if (y->p == z) {
      x->p = y;
    } else {
      transplant(T, y, y->right);
      y->right = z->right;
      y->right->p = y;
    }
    transplant(T, z, y);
    y->left = z->left;
    y->left->p = y;
    y->color = z->color;
  }
  if (y_original_color == BLACK) {
    delete_fixup(T, x);
  }
  T->count--;
}
