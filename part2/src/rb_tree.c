#define BLACK 0
#define RED 1

#include <stdlib.h>

typedef int color;
typedef struct node node;

// NODE TYPE, CLRS pp. 309

struct node {
  color color;
  int key; 
  node* right; 
  node* left;
  node* p;
};

node* make_node(int key) {
  node* res = (node*) malloc(sizeof(node));
  res->color = BLACK;
  res->key = key;
  res->right = NULL;
  res->left = NULL;
  res->p = NULL;
  return res;
}

node* make_sentinel() {
  return make_node(0);
}

// TREE TYPE

typedef struct tree tree;
struct tree {
  node* nil;
  node* root;
};

tree* make_tree() {
  tree* res = (tree*) malloc(sizeof(tree));
  res->nil = make_sentinel();
  res->root = res->nil;
  return res;
}

// PRIVATE UTILITIES

void left_rotate(tree* T, node* x) {
  node* y = x->right;
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

void right_rotate(tree* T, node* y) {
  node* x = y->left;
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

void insert_fixup(tree* T, node* z) {
  while (z->p->color == RED) {
    if (z->p == z->p->p->left) {
      node* y = z->p->p->right;
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
	node* y = z->p->p->left;
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

void transplant(tree* T, node* u, node* v) {
  if (u->p == T->nil) {
    T->root = v;
  } else if (u == u->p->left) {
    u->p->left = v;
  } else {
    u->p->right = v;
  }
  v->p = u->p;
}

void delete_fixup(tree* T, node* x) {
  while (x != T->root && x->color == BLACK) {
    if (x == x->p->left) {
      node* w = x->p->right;
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
      node* w = x->p->left;
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

node* minimum(tree* T, node* x) {
  while (x->left != T->nil) {
    x = x->left;
  }
  return x;
}   

node* maximum(tree* T, node* x) {
  while (x->right != T->nil) {
    x = x->right;
  }
  return x;
}

node* successor(tree* T, node* x) {
  if (x->right != T->nil) {
    return minimum(T, x->right);
  }
  node* y = x->p;
  while (y != T->nil && x == y->right) {
    x = y;
    y = y->p;
  }
  return y;
}

node* predecessor(tree* T, node* x) {
  if (x->left != T->nil) {
    return maximum(T, x->left);
  }
  node* y = x->p;
  while(y != T->nil && x == y->left) {
    x = y;
    y = y->p;
  } 
  return y;
}

node* search(tree* T, int k) {
  node* x = T->root;
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

void insert (tree* T, node* z) {
  node* y = T->nil;
  node* x = T->root;
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
}
	  
void delete(tree* T, node* z) {
  node* y = z;
  color y_original_color = y->color;
  node* x = NULL;
  if (z->left == T->nil) {
    x = z->right;
    transplant(T, z, z->right);
  } else if (z->right == T->nil) {
    x = z->left;
    transplant(T, z, z->left);
  } else {
    y = minimum(T, z->right);
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
}
