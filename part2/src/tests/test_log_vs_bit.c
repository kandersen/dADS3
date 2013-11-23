#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include "timer.h"

uint32_t u = 1 << 24;
uint32_t result;

static inline uint32_t log2_x86(const uint32_t x) {
  uint32_t y;
  asm ( "\tbsr %1, %0\n"
      : "=r"(y)
      : "r" (x)
  );
  return y;
}

void test_1() {
  result = pow(2, (int)ceil((log2 (u))/2));
}

void test_2() {
  result = 1<<(int)ceil((log2 (u))/2);
}

void test_3() {
  uint32_t b = log2 (u);
  uint32_t h = (b >> 1) + (b & 1);
  result = 1<<h;
}

void test_4() {
  result = 1<<(int)ceil((log2_x86 (u))/2);
}

void test_5() {
  uint32_t b = log2_x86 (u);
  uint32_t h = (b >> 1) + (b & 1);
  result = 1<<h;
}

int main (int argc, char** v) {
  void (*f[5]) (void) = {test_1, test_2, test_3, test_4, test_5};  
  for (int i = 0; i < 5; i++) {
    uint64_t total = 0;
    for (int j = 1; j < 1000000; j++) {      
      u = 1 << (j % 25);
      total += measure_function(f[i]);
    }
    printf("%d %lld\n", i, total / 1000000);
  }
}
