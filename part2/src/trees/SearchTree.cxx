#include "Item.cxx"
template <class Item, class Key>
class ST {
private:
  struct node {
    Item item;
    node* l;
    node* r;
    int red;
    node(Item x) {
      red = 0;
      item = x;
      l = 0;
      r = 0;
    } 
  }; 

  typedef node *link;

  link head;
  Item nullItem;  

  int red(link x) {
    if (x == 0) {
      return 0;
    } else {
      return x->red;
    }
  }

  void rotR(link& h) {
    link x = h->l;
    h->l = x->r;
    x->r = h;
    h = x;
  }
  
  void rotL(link& h) {
    link x = h->r;
    h->r = x->l;
    x->l = h;
    h = x;
  }
  
  void RBinsert(link& h, Item x, int sw) { 
    if (h == 0) {
      h = new node(x);
      return;
    }
    if (red(h->l) && red(h->r)) {
      h->red = 1;
      h->l->red = 0;
      h->r->red = 0;
    }
    if (x.key() < h->item.key()) {
      RBinsert(h->l, x, 0); 
      if (red(h) && red(h->l) && sw)
	rotR(h); 
      if (red(h->l) && red(h->l->l)) {
	rotR(h);
	h->red = 0;
	h->r->red = 1;
      }
    } else { 
      RBinsert(h->r, x, 1); 
      if (red(h) && red(h->r) && !sw)
	rotL(h); 
      if (red(h->r) && red(h->r->r)) {
	rotL(h);
	h->red = 0;
	h->l->red = 1;
      }
    }
  }

  Item searchR(link h, Key v) {
    if (h == 0)
      return nullItem;
    Key t = h->item.key();
    if (v == t)
      return h->item;
    if (v < t) {
      return searchR(h->l, v);
    } else {
      return searchR(h->r, v);
    }
  }
  
public:
  ST(int maxN) {
    head = 0;
  }
    
  Item search(Key k) {
    return searchR(head, k);
  }
  
  void insert(Item x) {
    RBinsert(head, x, 0);
    head->red = 0;
  }
  
  //void remove(Item)
  //Item select(int)
  //void show(ostream&)
};
