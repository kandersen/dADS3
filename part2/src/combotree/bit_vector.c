#include "bit_vector.h"

typedef struct bit_vector bit_vector;
typedef struct vEB_tree vEB_tree;

uint32_t
bit_count(uint8_t const universe_bits)
{
  return sizeof(block_type) * 8;
}

uint32_t
block_count(uint8_t const universe_bits)
{
     uint32_t vector_bits = 1 << universe_bits;
     uint32_t bit_count = sizeof(block_type) * 8;
     uint32_t block_count = ceil(vector_bits / (sizeof(block_type) * 8.0f));
     return block_count;
}

bit_vector *
bit_vector_init (uint8_t const universe_bits) 
{
     uint32_t vector_bits = 1 << universe_bits;
     uint32_t bit_count = sizeof(block_type) * 8;
     uint32_t block_count = ceil(vector_bits / (sizeof(block_type) * 8.0f));
     
     bit_vector const_initialization = {
          .universe_bits = universe_bits, 
          .min           = none(),
          .max           = none(),
          .blocks        = malloc(sizeof(block_type) * block_count)
     };

     bit_vector * vector = ( bit_vector *) malloc(sizeof( bit_vector));
     memcpy(vector, &const_initialization, sizeof( bit_vector));
     memset(vector->blocks, 0, sizeof(block_type) * block_count);

     return vector;
}

bool
bit_vector_insert (vEB_tree * const vector, uint24_option const value) 
{
     if (is_none(value))
          return false;     

     uint32_t bit_number = value & (bit_count(vector->universe_bits) - 1);
     uint32_t block_number = value / bit_count(vector->universe_bits);
     uint32_t already_contains_value = vEB_contains(vector, value);

     vector->blocks[block_number] |= (1 << bit_number);

     if (is_none(vector->min) || value < vector->min)
          vector->min = some(value);
     
     if (is_none(vector->max) || value > vector->max)
          vector->max = some(value);
     
     return !already_contains_value;
}

bool
bit_vector_delete (vEB_tree * const vector, uint24_option const value) 
{
     if (is_none(value))
          return false;

     uint32_t bit_number = value & (bit_count(vector->universe_bits) - 1);
     uint32_t block_number = value / bit_count(vector->universe_bits);
     uint32_t already_vacant_value = !vEB_contains(vector, value);

     vector->blocks[block_number] &= ~((block_type) (1 << bit_number));
     
     if (vector->min == value && vector->max == value)
          return vector->min = vector->max = none(), !already_vacant_value;

     if (vector->min == value)
          vector->min = vEB_succ(vector, vector->min);

     if (vector->max == value)
          vector->max = vEB_pred(vector, vector->max);

     return !already_vacant_value;
}

bool
bit_vector_contains (vEB_tree const * const vector, uint24_option const value)
{
     if (is_none(value))
          return false;

     uint32_t bit_number = value & (bit_count(vector->universe_bits) - 1);
     uint32_t block_number = value / bit_count(vector->universe_bits);
     uint32_t mask = 1 << bit_number;

     bool res = !!(vector->blocks[block_number] & mask);
     return res;
}

uint24_option
bit_vector_minimum (vEB_tree const * const vector)
{
     return some(vector->min);
}

uint24_option
bit_vector_maximum (vEB_tree const * const vector)
{
     return some(vector->max);
}

uint24_option
bit_vector_pred (vEB_tree const * const vector, uint24_option const value)
{
     if (is_none(value))
          return none();

     uint24_option min_element = some(0);
     uint24_option max_element = some(((1 << vector->universe_bits)-1));
     uint24_option test_value;
    
     for (test_value = some(value) - 1; test_value >= min_element && test_value < max_element; test_value--)
         if (vEB_contains(vector, test_value))
           return some(test_value);

     return none();
}

uint24_option
bit_vector_succ (vEB_tree const * const vector, uint24_option const value)
{
     if (is_none(value))
          return none();

     uint24_option max_element = some((1 << vector->universe_bits)-1);
     uint24_option test_value;

     for (test_value = some(value) + 1; test_value <= max_element; test_value++)
         if (vEB_contains(vector, test_value))
           return some(test_value);

     return none();
}
