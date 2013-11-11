#ifndef DADS3_BIT_VECTOR_H
#define DADS3_BIT_VECTOR_H

#include "int_option.h"
#include "veb_tree.h"
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <math.h>
#include <stdbool.h>

typedef int block_type;

struct vEB_tree;

struct bit_vector {
  uint8_t const universe_bits;
  uint24_option min, max;
  block_type* blocks;
};

struct bit_vector * bit_vector_init (uint8_t const);
bool bit_vector_insert (struct vEB_tree *const, uint24_option const);
bool bit_vector_delete (struct vEB_tree *const, uint24_option const);
bool bit_vector_contains (struct vEB_tree const *const, uint24_option const);
uint24_option bit_vector_minimum (struct vEB_tree const *const);
uint24_option bit_vector_maximum (struct vEB_tree const *const);
uint24_option bit_vector_pred (struct vEB_tree const *const, uint24_option const);
uint24_option bit_vector_succ (struct vEB_tree const *const, uint24_option const);

#endif
