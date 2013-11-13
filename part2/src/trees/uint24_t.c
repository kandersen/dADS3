#include "uint24_t.h"

uint32_t
uint24_as_uint32(uint24_t uint24)
{
  uint32_t uint32 = 0;
  
  uint32 += (uint24.bytes[0] << 0);
  uint32 += (uint24.bytes[1] << 8);
  uint32 += (uint24.bytes[2] << 16);
  
  return uint32;
}

uint24_t
uint32_as_uint24(uint32_t uint32)
{
  uint24_t uint24;

  uint24.bytes[0] = (uint32 >>  0) & 0xFF;
  uint24.bytes[1] = (uint32 >>  8) & 0xFF;
  uint24.bytes[2] = (uint32 >> 16) & 0xFF;
  
  return uint24;
}

