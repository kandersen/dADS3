
#ifndef DADS3_TREE_H
#define DADS3_TREE_H

#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <math.h>

#include "int_option.h"

#define _KIND_BIT 0
#define _KIND_VEB 1

typedef struct tree tree;

struct tree * make_tree (uint8_t const);
bool insert_item (struct tree *const, uint24_option const);
bool delete_item (struct tree *const, uint24_option const);
bool contains (struct tree const *const, uint24_option const);
uint24_option minimum (struct tree const *const);
uint24_option maximum (struct tree const *const);
uint24_option pred (struct tree const *const, uint24_option const);
uint24_option succ (struct tree const *const, uint24_option const);


#endif
