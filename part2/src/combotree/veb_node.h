#ifndef DADS3_VEB_NODE_H
#define DADS3_VEB_NODE_H

#include "int_option.h"
#include "veb_tree.h"

#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <math.h>
#include <string.h>
#include <stdbool.h>

struct vEB_tree;
struct vEB_node {
     uint8_t const universe_bits;
     uint24_option min, max;
     struct vEB_tree **const bottom;
     struct vEB_tree  *const top;
};

struct vEB_node * vEB_node_init (uint8_t const);
bool vEB_node_insert (struct vEB_tree *const, uint24_option const);
bool vEB_node_delete (struct vEB_tree *const, uint24_option const);
bool vEB_node_contains (struct vEB_tree const *const, uint24_option const);
uint24_option vEB_node_minimum (struct vEB_tree const *const);
uint24_option vEB_node_maximum (struct vEB_tree const *const);
uint24_option vEB_node_pred (struct vEB_tree const *const, uint24_option const);
uint24_option vEB_node_succ (struct vEB_tree const *const, uint24_option const);


#endif
