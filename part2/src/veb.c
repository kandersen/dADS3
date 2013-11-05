#include <math.h>
#include <stdlib.h>
#include "int_option.h"

struct vEB_tree {
  int_option min;
  int_option max;
  int u;
  struct vEB_tree** bottom;
  struct vEB_tree* top;
};

typedef struct vEB_tree vEB_tree;

// UTILITIES


int upper(int u) {
  return pow(2,(int)ceil((log2 (u))/2));
}

int lower(int u) {
  return pow(2,(int)((log2 (u))/2));
}

int high(int x, vEB_tree* veb) {
  return (int)(x / lower(veb->u));
}

int low(int x, vEB_tree* veb) {
  return x % lower(veb->u);
}

int find_index(int x, int y, vEB_tree* veb) {
  return x * lower(veb->u) + y;
}

vEB_tree* make_veb(int u) {

  vEB_tree* t = (vEB_tree*)malloc(sizeof(vEB_tree));
  t->u = u;
  t->min = none();
  t->max = none();
  t->bottom = NULL;

  // construct clusters recursively?
  if (u > 2) { 
    int up = upper(u);
    t->bottom = (vEB_tree**)malloc(sizeof(vEB_tree*) * up);
    for (int i = 0; i < up; i++) {
      t->bottom[i] = make_veb(up);
    }

    // how to construct top, is it just a smaller table of sqrt(u)?
    t->top = make_veb(up);
  }

  return t;
}

int_option minimum(vEB_tree* veb) {
  return veb->min;
}

int_option maximum(vEB_tree* veb) {
  return veb->max;
}

void empty_tree_insert(int x, vEB_tree* veb) {
  veb->min = some(x);
  veb->max = some(x);
}

void insert_item(int x, vEB_tree* veb) {
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
        insert_item(h, veb->top);
        empty_tree_insert(l, veb->bottom[h]);
      } else {
        insert_item(l, veb->bottom[h]);
      }
    }
    if (x > veb->max) {
      veb->max = some(x);
    }
  }
}

void remove_item(int x, vEB_tree* veb) {
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
    remove_item(l, veb->bottom[h]);
    if (is_none(minimum(veb->bottom[h]))) {
      remove_item(h, veb->top);
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

void delete_min(vEB_tree* veb) {
  int_option min = minimum(veb);
  if (is_some(min)) {
    remove_item(min, veb);
  }
}

int_option predecessor(int x, vEB_tree* veb) {
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

int_option successor(int x, vEB_tree* veb) {
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

int member(int x, vEB_tree* veb) {

  if (x == veb->min || x == veb->max) {
    return 1; // true
  } else if (veb->u == 2) {
    return 0; // false
  } else {
    return member(low(x, veb), veb->bottom[high(x, veb)]);
  }
}


int main(int argc, char** argv) {

}
