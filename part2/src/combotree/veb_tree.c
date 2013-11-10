#include "veb_tree.h"
#include <assert.h>
#ifndef MIN_VEB_UNIVERSE_BITS
#define MIN_VEB_UNIVERSE_BITS 4
#endif

typedef struct vEB_node vEB_node;
typedef struct vEB_tree vEB_tree;
typedef struct bit_vector bit_vector;

vEB_tree * vEB_init (uint8_t const universe_bits)
{
     vEB_tree * tree = malloc(sizeof(vEB_tree));
     
     if (universe_bits < MIN_VEB_UNIVERSE_BITS) 
     {
          tree->kind = _KIND_BIT;
          tree->bit_vector = bit_vector_init(universe_bits);
     }
     else
     {
          tree->kind = _KIND_VEB;
          tree->vEB_node = vEB_node_init(universe_bits);
     }
     return tree;
}

bool vEB_insert (vEB_tree *const tree, uint24_option const value)
{
     bool (*insert[2]) (vEB_tree *const, uint24_option const);
     insert[_KIND_BIT] = bit_vector_insert;
     insert[_KIND_VEB] = vEB_node_insert;

     return insert[tree->kind] (tree, value);
}

bool vEB_delete (vEB_tree *const tree, uint24_option const value)
{
     bool (*delete[2]) (vEB_tree *const, uint24_option const);
     delete[_KIND_BIT] = bit_vector_delete;
     delete[_KIND_VEB] = vEB_node_delete;

     return delete[tree->kind] (tree, value);
}

bool vEB_contains (vEB_tree const *const tree, uint24_option const value)
{
     bool (*contains[2]) (vEB_tree const *const, uint24_option const);
     contains[_KIND_BIT] = bit_vector_contains;
     contains[_KIND_VEB] = vEB_node_contains;

     return contains[tree->kind] (tree, value);
}

uint24_option vEB_minimum (vEB_tree const *const tree)
{
     uint24_option (*minimum[2]) (vEB_tree const *const);
     minimum[_KIND_BIT] = bit_vector_minimum;
     minimum[_KIND_VEB] = vEB_node_minimum;

     return minimum[tree->kind] (tree);
}

uint24_option vEB_maximum (vEB_tree const *const tree)
{
     uint24_option (*maximum[2]) (vEB_tree const *const);
     maximum[_KIND_BIT] = bit_vector_maximum;
     maximum[_KIND_VEB] = vEB_node_maximum;

     return maximum[tree->kind] (tree);
}

uint24_option vEB_pred (vEB_tree const *const tree, uint24_option const value)
{
     uint24_option (*pred[2]) (vEB_tree const *const, uint24_option const);
     pred[_KIND_BIT] = bit_vector_pred;
     pred[_KIND_VEB] = vEB_node_pred;
     return pred[tree->kind] (tree, value);
}

uint24_option vEB_succ (vEB_tree const *const tree, uint24_option const value)
{
     uint24_option (*succ[2]) (vEB_tree const * const, uint24_option const);
     succ[_KIND_BIT] = bit_vector_succ;
     succ[_KIND_VEB] = vEB_node_succ;
     return succ[tree->kind] (tree, value);
}


int main(int a, char** c) {
     printf("%d\n", MIN_VEB_UNIVERSE_BITS);
     vEB_tree * t = vEB_init(24);

     assert(vEB_minimum(t) == none());
     assert(vEB_maximum(t) == none());
     assert(vEB_contains(t, some(41)) == false);
     assert(vEB_contains(t, some(24)) == false);   
     assert(vEB_insert(t, some(24)));
     assert(vEB_contains(t, some(24)));
     assert(vEB_minimum(t) == some(24));
     assert(vEB_maximum(t) == some(24));
     assert(vEB_insert(t, some(23)));
     assert(vEB_insert(t, some(22)));
     assert(vEB_insert(t, some(25)));
     assert(vEB_minimum(t) == some(22));
     assert(vEB_maximum(t) == some(25));
     assert(vEB_delete(t, some(42)) == false);    
     assert(vEB_delete(t, some(22)));
     assert(vEB_minimum(t) == some(23));
     assert(vEB_delete(t, some(25))); // BAD
     assert(vEB_delete(t, some(25)) == false);
     assert(vEB_maximum(t) == some(24));

}
