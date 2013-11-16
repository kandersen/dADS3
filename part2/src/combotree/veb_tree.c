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
          bit_vector * bt = bit_vector_init(universe_bits);
          tree->kind = _KIND_BIT;
          tree->min = bt->min;
          tree->max = bt->max;
          tree->blocks = bt->blocks;
          tree->universe_bits = bt->universe_bits;
     }
     else
     {
          vEB_node * n = vEB_node_init(universe_bits);
          tree->kind = _KIND_VEB;
          tree->min = n->min;
          tree->max = n->max;
          tree->universe_bits = n->universe_bits;
          tree->bottom = n->bottom;
          tree->top = n->top;
     }
     
     
     return tree;
}

bool vEB_insert (vEB_tree *const tree, uint24_option const value)
{
     
     bool (*insert[2]) (vEB_tree *const, uint24_option const);
     insert[_KIND_BIT] = bit_vector_insert;
     insert[_KIND_VEB] = vEB_node_insert;
     bool success = insert[tree->kind] (tree, value);
     
     return success;
}

bool vEB_delete (vEB_tree *const tree, uint24_option const value)
{
     
     bool (*delete[2]) (vEB_tree *const, uint24_option const);
     delete[_KIND_BIT] = bit_vector_delete;
     delete[_KIND_VEB] = vEB_node_delete;

     bool success =  delete[tree->kind] (tree, value);
     
     return success;
}

bool vEB_contains (vEB_tree const *const tree, uint24_option const value)
{
     bool (*contains[2]) (vEB_tree const *const, uint24_option const);
     contains[_KIND_BIT] = bit_vector_contains;
     contains[_KIND_VEB] = vEB_node_contains;

     bool success = contains[tree->kind] (tree, value);
     
     return success;
}

uint24_option vEB_minimum (vEB_tree const *const tree)
{
     
     uint24_option (*minimum[2]) (vEB_tree const *const);
     minimum[_KIND_BIT] = bit_vector_minimum;
     minimum[_KIND_VEB] = vEB_node_minimum;
     uint24_option res = minimum[tree->kind] (tree);
     
     return res;
}

uint24_option vEB_maximum (vEB_tree const *const tree)
{
     
     uint24_option (*maximum[2]) (vEB_tree const *const);
     maximum[_KIND_BIT] = bit_vector_maximum;
     maximum[_KIND_VEB] = vEB_node_maximum;

     uint24_option res = maximum[tree->kind] (tree);
     
     return res;
}

uint24_option vEB_pred (vEB_tree const *const tree, uint24_option const value)
{
     
     uint24_option (*pred[2]) (vEB_tree const *const, uint24_option const);
     pred[_KIND_BIT] = bit_vector_pred;
     pred[_KIND_VEB] = vEB_node_pred;
     uint24_option res = pred[tree->kind] (tree, value);
     
     return res;
}

uint24_option vEB_succ (vEB_tree const *const tree, uint24_option const value)
{
     
     uint24_option (*succ[2]) (vEB_tree const * const, uint24_option const);
     succ[_KIND_BIT] = bit_vector_succ;
     succ[_KIND_VEB] = vEB_node_succ;
     uint24_option res = succ[tree->kind] (tree, value);
     
     return res;
}
