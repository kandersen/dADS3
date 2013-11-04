#include <math.h>
#include <stdlib.h>

struct vEB_tree {
  int* min;
  int* max;
  int u;
  vEB_tree** bottom;
  vEB_tree* top;
};


int upper(int u) {
  return pow(2,(int)ceil((lg u)/2));
}

int lower(int u) {
  return pow(2,(int)((lg u)/2));
}

int high(int x, vEB_tree veb) {
  return (int)(x / lower(veb->u));
}

int low(int x, vEB_tree veb) {
  return x % lower(veb->u);
}

int* index(int x, int y, vEB_tree veb) {
  return x * lower(veb->u) + y;
}

vEB_tree* make_veb(int u) {

  vEB_tree* t = (vEB_tree*)malloc(sizeof(vEB_tree));
  t->u = u;
  t->min = NULL;
  t->max = NULL;
  t->bottom = NULL;

  // construct clusters recursively?
  if (u > 2) { 
    int upper = upper(u);
    t->bottom = (vEB_tree**)malloc(sizeof(vEB_tree*) * upper);
    for (int i = 0; i < upper; i++) {
      t->bottom[i] = make_veb(upper);
    }

  // how to construct top, is it just a smaller table of sqrt(u)?

  }
}

int* minimum(vEB_tree* veb) {
  return veb->min;
}

int* maximum(vEB_tree* veb) {
  return veb->max;
}

void empty_tree_insert(int x, vEB_tree* veb) {
  *veb->min = x; // HVOR SKAL DER VÆRE MALLOC?
  *veb->max = x;
}

void insert_item(int x, vEB_tree* veb) {
  if (veb->min == NULL) {
    empty_tree_insert(x, veb);
  } else {
    if (x < *veb->min) {
      int temp = *veb->min;
      *veb->min = x;
      x = temp;
    } 
    if (veb->u > 2) {
      int high = high(x, veb);
      int low = low(x, veb);
      if (minimum(veb->bottom[high]) == NULL) {
        insert_item(high, veb->top);
        empty_tree_insert(low, veb->bottom[high]);
      } else {
        insert_item(low, veb->bottom[high]);
      }
    }
    if (x > *veb->max) {
      *veb->max = x
    }
  }
}

void remove_item(int x, vEB_tree* veb) {
  if (veb->min == NULL) {
    return;
  }

  if (veb->veb->min == veb->max) {
    veb->min = NULL;
    veb->max = NULL;
  } else if (veb->u == 2) {
    if (x == 0) {
      *veb->min = 1;
    } else {
      *veb->min = 0;
    }
    *veb->max = veb->min;
  } else {
    if (x == *veb->min) {
      int* first_cluster = minimum(veb->top);
      x = index(*first_cluster, minimum(veb->bottom[*first_cluster]), veb);
      *veb->min = x;
    }
    int high = high(x, veb);
    int low = low(x, veb);
    remove_item(low, veb->bottom[high]);
    if (minimum(veb->bottom[high]) == NULL) {
      remove_item(high, veb->top);
      if (x == *veb->max) {
        int* top_max = maximum(veb->top);
        if (top_max != NULL) {
          *veb->max = *veb->min;
        } else {
          *veb->max = index(*top_max, *maximum(veb->bottom[*top_max]), veb);
        }
      }
    } else if (x == *veb->max) {
      veb->max = index(high, maximum(veb->bottom[high]), veb);
    }
  }
}

void delete_min(vEB_tree* veb) {
  int* min = minimum(veb);
  if (min != NULL) {
    remove_item(*min);
  }
}

item* predecessor(int x, vEB_tree* veb) {
  if (veb->u == 2) {
    if (x == 1 && veb->min != NULL && *veb->min == 0) {
      return 0;
    } else {
      return NULL;
    }
  } else if (veb->max != NULL && x > *veb->max)  {
    return veb->max;
  } else {
    int high = high(x, veb);
    int low = low(x, veb);
    int* min_low = minimum(veb->bottom[high]);
    if (min_low != NULL && low > *min_low) {
      int* offset = predecessor(low, veb->bottom[high]);
      return index(high, *offset, veb);
    } else {
      int* pred_cluster = predecessor(high, veb->top);
      if (pred_cluster == NULL) {
        if (veb->min != NULL && x > *veb->min) {
          return veb->min;
        } else { 
          return NULL;
        }
      } else {
        int* offset = maximum(veb->bottom[*pred_cluster]);
        return index(*pred_cluster, *offset, veb);
      }
    }
  }

}

int* successor(int x, vEB_tree* veb) {
  if (veb->u == 2) {
    if (x == 0 && veb->max != NULL && *veb->max == 1) {
      return 1;
    } else {
      return NULL;
    }
  } else if (veb->min != NULL && x < *veb->min) {
    return veb->min;
  } else {
    int high = high(x, veb);
    int low = low(x, veb);
    int* max_low = maximum(veb->bottom[high]);
    if (max_low != NULL && low < *max_low) {
      int* offset = successor(low, veb->bottom[high]);
      return index(high, *offset, veb);
    } else {
      int* succ_cluster = successor(high, veb->top);
      if (succ_cluster == NULL) {
        return NULL;
      } else {
        int* offset = minimum(veb->bottom[*succ_cluster]);
        return index(*succ_cluster, *offset);
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
