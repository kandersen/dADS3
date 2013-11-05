#ifndef DADS3_UINT24_H
#define DADS3_UINT24_H

#include <stdint.h>

struct uint24_t {
  char bytes[3];
};

typedef struct uint24_t uint24_t; 
uint32_t uint24_as_uint32 (uint24_t);
uint24_t uint32_as_uint24 (uint32_t);

#endif
