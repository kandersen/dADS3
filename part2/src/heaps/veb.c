#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include "int_option.h"
#include "heap.h"

struct heap {
  int_option min;
  int_option max;
  int u;
  int bottom_size;
  struct heap** bottom;
  struct heap* top;
};

// UTILITIES


int upper(int u) {
  return pow(2,(int)ceil((log2 (u))/2));
}

int lower(int u) {
  return pow(2,(int)((log2 (u))/2));
}

int high(int x, heap* veb) {
  return (int)(x / lower(veb->u));
}

int low(int x, heap* veb) {
  return x % lower(veb->u);
}

int find_index(int x, int y, heap* veb) {
  return x * lower(veb->u) + y;
}

heap* make_veb(int u) {

  heap* t = (heap*)malloc(sizeof(heap));
  t->u = u;
  t->min = none();
  t->max = none();
  t->bottom = NULL;
  t->bottom_size = 0;

  // construct clusters recursively?
  if (u > 2) { 
    int up = upper(u);
    t->bottom_size = up;
    t->bottom = (heap**)malloc(sizeof(heap*) * up);
    for (int i = 0; i < up; i++) {
      t->bottom[i] = make_veb(up);
    }

    // how to construct top, is it just a smaller table of sqrt(u)?
    t->top = make_veb(up);
  }

  return t;
}

heap* make_heap(int universe) {
  return make_veb(universe); 
}

int_option minimum(heap* veb) {
  return veb->min;
}

int_option maximum(heap* veb) {
  return veb->max;
}

void empty_tree_insert(int x, heap* veb) {
  veb->min = some(x);
  veb->max = some(x);
}

void insert_item(item* ix, heap* veb) {

  int x = ix->key;

  if (x >= veb->u) {
    printf("ALARM ALARM: input større end univers: %d", x);
  }

  if (is_none(veb->min)) {
    empty_tree_insert(x, veb);
  } else {
    if (x < veb->min) {
      int temp = veb->min;
      veb->min = some(x);
      x = temp;
    } 
    if (veb->u > 2) {
      int h = high(x, veb);
      int l = low(x, veb);
      if (is_none(minimum(veb->bottom[h]))) {
        ix->key = h;
        insert_item(ix, veb->top);
        empty_tree_insert(l, veb->bottom[h]);
      } else {
        ix->key = l;
        insert_item(ix, veb->bottom[h]);
      }
    }
    if (x > veb->max) {
      veb->max = some(x);
    }
  }
}

void remove_item(item* ix, heap* veb) {
  
  int x = ix->key;

  if (is_none(veb->min)) {
    return;
  }

  if (veb->min == veb->max) {
    veb->min = none();
    veb->max = none();
  } else if (veb->u == 2) {
    if (x == 0) {
      veb->min = some(1);
    } else {
      veb->min = some(0);
    }
    veb->max = veb->min;
  } else {
    if (x == veb->min) {
      int_option first_cluster = minimum(veb->top);
      x = find_index(first_cluster, minimum(veb->bottom[first_cluster]), veb);
      veb->min = some(x);
    }
    int h = high(x, veb);
    int l = low(x, veb);
    ix->key = l;
    remove_item(ix, veb->bottom[h]);
    if (is_none(minimum(veb->bottom[h]))) {
      ix->key = h;
      remove_item(ix, veb->top);
      if (x == veb->max) {
        int_option top_max = maximum(veb->top);
        if (is_none(top_max)) {
          veb->max = veb->min;
        } else {
          veb->max = some(find_index(top_max, maximum(veb->bottom[top_max]), veb));
        }
      }
    } else if (x == veb->max) {
      veb->max = some(find_index(h, maximum(veb->bottom[h]), veb));
    }
  }
}

void decrease_key(int delta, item* i, heap* h) {
  int val = i->key;
  remove_item(i, h);
  i->key = val - delta;
  insert_item(i, h);
}

item* delete_min(heap* veb) {
  int_option min = minimum(veb);
  if (is_some(min)) {
    item* itm = (item*)malloc(sizeof(item));
    itm->key = min;
    remove_item(itm, veb);
    return itm;
  } else {
    return NULL;
  }
}

int_option predecessor(int x, heap* veb) {
  if (veb->u == 2) {
    if (x == 1 && veb->min == 0) {
      return some(0);
    } else {
      return none();
    }
  } else if (is_none(veb->max) && x > veb->max)  {
    return veb->max;
  } else {
    int h = high(x, veb);
    int l = low(x, veb);
    int_option min_low = minimum(veb->bottom[h]);
    if (is_some(min_low) && l > min_low) {
      int_option offset = predecessor(l, veb->bottom[h]);
      return find_index(h, offset, veb);
    } else {
      int_option pred_cluster = predecessor(h, veb->top);
      if (is_none(pred_cluster)) {
        if (is_some(veb->min) && x > veb->min) {
          return veb->min;
        } else { 
          return none();
        }
      } else {
        int_option offset = maximum(veb->bottom[pred_cluster]);
        return find_index(pred_cluster, offset, veb);
      }
    }
  }
}

int_option successor(int x, heap* veb) {
  if (veb->u == 2) {
    if (x == 0 && veb->max == 1) {
      return some(1);
    } else {
      return none();
    }
  } else if (is_some(veb->min) && x < veb->min) {
    return veb->min;
  } else {
    int h = high(x, veb);
    int l = low(x, veb);
    int_option max_low = maximum(veb->bottom[h]);
    if (is_some(max_low) && l < max_low) {
      int_option offset = successor(l, veb->bottom[h]);
      return find_index(h, offset, veb);
    } else {
      int_option succ_cluster = successor(h, veb->top);
      if (is_none(succ_cluster)) {
        return none();
      } else {
        int_option offset = minimum(veb->bottom[succ_cluster]);
        return find_index(succ_cluster, offset, veb);
      }
    }
  }
}

int member(int x, heap* veb) {

  if (x == veb->min || x == veb->max) {
    return 1; // true
  } else if (veb->u == 2) {
    return 0; // false
  } else {
    return member(low(x, veb), veb->bottom[high(x, veb)]);
  }
}

void test_int_option() {

  int_option x = 1337;
  int_option y = some(1337);
  printf("x: %d\n", x);
  printf("y: %d\n", y);

}

item* create_item_dummy(int key) {
  item* itm = (item*)malloc(sizeof(item));
  itm->key = key;
  return itm;
}

/*
int main(int argc, char** argv) {

  test_int_option();

  int size = 16;
  heap* t = make_veb(size);
  insert_item(create_item_dummy(2), t);
  insert_item(create_item_dummy(6), t);
  printf("minimum %d\n", minimum(t));
  printf("maximum %d\n", maximum(t));
  printf("member %d\n", member(6, t));
  printf("not-member %d\n", member(2, t));
  if (is_some(successor(4, t))) {
    printf("successor of 4: %d\n", successor(4, t));
  } else {
    printf("no-successor for 4\n");
  }
  if (is_some(successor(8, t))) {
    printf("successor of 8: %d\n", successor(8, t));
  } else {
    printf("no-successor for 8\n");
  }
  if (is_some(predecessor(10, t))) {
    printf("predecessor of 10: %d\n", predecessor(10, t));
  } else {
    printf("no-predecessor for 10\n");
  }
  remove_item(create_item_dummy(2), t);
  printf("minimum %d\n", minimum(t));
  printf("maximum %d\n", maximum(t));  
  remove_item(create_item_dummy(6), t);
  printf("minimum %d\n", minimum(t));
  printf("maximum %d\n", maximum(t));  
  remove_item(create_item_dummy(10), t);
  } */
