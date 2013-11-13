#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include "veb_only.h"

struct tree {
  uint24_option min, max;
  int u;
  int bottom_size;
  struct tree** bottom;
  struct tree* top;
};

// UTILITIES


int upper(uint24_option const u) {
  return pow(2,(int)ceil((log2 (u))/2));
  // CHANGE this
}

int lower(uint24_option const u) {
  // CHANGE this
  return pow(2,(int)((log2 (u))/2));
}

int high(uint24_option const x, tree const* veb) {
  // CHANGE this
  return (int)(x / lower(veb->u));
}

int low(uint24_option const x, tree const* veb) {
  // CHANGE this
  return x % lower(veb->u);
}

int find_index(uint24_option const x, uint24_option const y, tree const* veb) {
  return x * lower(veb->u) + y;
}

tree* make_veb(int u) {

  tree* t = (tree*)malloc(sizeof(tree));
  t->u = u;
  t->min = none();
  t->max = none();
  t->bottom = NULL;
  t->bottom_size = 0;

  // construct clusters recursively
  if (u > 2) { 
    int up = upper(u);
    int lo = lower(u);
    t->bottom_size = up;
    t->bottom = (tree**)malloc(sizeof(tree*) * up);
    for (int i = 0; i < up; i++) {
      t->bottom[i] = make_veb(lo);
    }

    t->top = make_veb(up);
  }

  return t;
}

tree* make_tree(uint8_t bits) {
  return make_veb((1 << bits));
}

uint24_option minimum(tree const* veb) {
  return veb->min;
}

uint24_option maximum(tree const* veb) {
  return veb->max;
}

void empty_tree_insert_item(int x, tree* veb) {
  veb->min = some(x);
  veb->max = some(x);
}

bool insert_item(tree* veb, uint24_option x) {
  if (is_none(veb->min)) {
    empty_tree_insert_item(x, veb);
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
        insert_item(veb->top, h);
        empty_tree_insert_item(l, veb->bottom[h]);
      } else {
        insert_item(veb->bottom[h], l);
      }
    }
    if (x > veb->max) {
      veb->max = some(x);
    }
  }
  return true;
}

bool delete_item(tree* veb, uint24_option x) {
  if (is_none(veb->min)) {
    return false;
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
      uint24_option first_cluster = minimum(veb->top);
      x = find_index(first_cluster, minimum(veb->bottom[first_cluster]), veb);
      veb->min = some(x);
    }
    int h = high(x, veb);
    int l = low(x, veb);
    delete_item(veb->bottom[h], l);
    if (is_none(minimum(veb->bottom[h]))) {
      delete_item(veb->top, h);
      if (x == veb->max) {
        uint24_option top_max = maximum(veb->top);
        if (is_none(top_max)) {
          veb->max = veb->min;
        } else {
          veb->max = some(find_index(top_max, maximum(veb->bottom[top_max]), veb));
        }
      }
    } else if (x == veb->max) {
      veb->max = some(find_index(h, minimum(veb->bottom[h]), veb));
    }
  }
  return true;
}

uint24_option pred(tree const* veb, uint24_option const x) {
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
    uint24_option min_low = minimum(veb->bottom[h]);
    if (is_some(min_low) && l > min_low) {
      uint24_option offset = pred(veb->bottom[h], l);
      return find_index(h, offset, veb);
    } else {
      uint24_option pred_cluster = pred(veb->top, h);
      if (is_none(pred_cluster)) {
        if (is_some(veb->min) && x > veb->min) {
          return veb->min;
        } else { 
          return none();
        }
      } else {
        uint24_option offset = maximum(veb->bottom[pred_cluster]);
        return find_index(pred_cluster, offset, veb);
      }
    }
  }
}

uint24_option succ(tree const* veb, uint24_option const x) {
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
    uint24_option max_low = maximum(veb->bottom[h]);
    if (is_some(max_low) && l < max_low) {
      uint24_option offset = succ(veb->bottom[h], l);
      return find_index(h, offset, veb);
    } else {
      uint24_option succ_cluster = succ(veb->top, h);
      if (is_none(succ_cluster)) {
        return none();
      } else {
        uint24_option offset = minimum(veb->bottom[succ_cluster]);
        return find_index(succ_cluster, offset, veb);
      }
    }
  }
}

bool contains(tree const* veb, uint24_option const x) {

  if (x == veb->min || x == veb->max) {
    return 1; // true
  } else if (veb->u == 2) {
    return 0; // false
  } else {
    return contains(veb->bottom[high(x, veb)], low(x, veb));
  }
}

/*
int main(int argc, char** argv) {

  int size = 12;
  tree* t = make_tree(size);

  insert_item(t, 2);
  insert_item(t, 6);
  printf("minimum %d\n", minimum(t));
  printf("maximum %d\n", maximum(t));
  printf("member %d\n", contains(t, 6));
  printf("not-member %d\n", contains(t, 3));
  if (is_some(succ(t, 4))) {
    printf("successor of 4: %d\n", succ(t, 4));
  } else {
    printf("no-successor for 4\n");
  }
  if (is_some(succ(t, 8))) {
    printf("successor of 8: %d\n", succ(t, 8));
  } else {
    printf("no-successor for 8\n");
  }
  if (is_some(pred(t, 10))) {
    printf("predecessor of 10: %d\n", pred(t, 10));
  } else {
    printf("no-predecessor for 10\n");
  }

  delete_item(t, 2);
  printf("minimum %d\n", minimum(t));
  printf("maximum %d\n", maximum(t));  
  delete_item(t, 6);
  printf("minimum %d\n", minimum(t));
  printf("maximum %d\n", maximum(t));  
  delete_item(t, 10);
}
*/
