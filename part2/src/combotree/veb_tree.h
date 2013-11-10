#ifndef DADS3_VEB_TREE_H
#define DADS3_VEB_TREE_H

#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <math.h>

#include "int_option.h"
#include "veb_node.h"
#include "bit_vector.h"


#define _KIND_BIT 0
#define _KIND_VEB 1

struct vEB_node;
struct vEB_tree 
{
  uint8_t kind;
  union {
    struct bit_vector* bit_vector;
    struct vEB_node* vEB_node;
  };
};


struct vEB_tree * vEB_init (uint8_t const);
bool vEB_insert (struct vEB_tree *const, uint24_option const);
bool vEB_delete (struct vEB_tree *const, uint24_option const);
bool vEB_contains (struct vEB_tree const *const, uint24_option const);
uint24_option vEB_minimum (struct vEB_tree const *const);
uint24_option vEB_maximum (struct vEB_tree const *const);
uint24_option vEB_pred (struct vEB_tree const *const, uint24_option const);
uint24_option vEB_succ (struct vEB_tree const *const, uint24_option const);


#endif
