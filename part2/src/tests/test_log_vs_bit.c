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

static const char LogTable256[256] = 
{
#define LT(n) n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n
    -1, 0, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3,
    LT(4), LT(5), LT(5), LT(6), LT(6), LT(6), LT(6),
    LT(7), LT(7), LT(7), LT(7), LT(7), LT(7), LT(7), LT(7)
};

unsigned int v; // 32-bit word to find the log of
unsigned r;     // r will be lg(v)
unsigned int t, tt; // temporaries

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

void test_6() {
  
if (tt = u >> 16)
{
  r = (t = tt >> 8) ? 24 + LogTable256[t] : 16 + LogTable256[tt];
}
else 
{
  r = (t = u >> 8) ? 8 + LogTable256[t] : LogTable256[u];
}
  uint32_t h = (r >> 1) + (r & 1);
  result = 1<<h;

}

int main (int argc, char** v) {
  test_1();
  printf("%d\n", result);
  test_2();
  printf("%d\n", result);
  test_3();
  printf("%d\n", result);
  test_4();
  printf("%d\n", result);
  test_5();
  printf("%d\n", result);
  test_6();
  printf("%d\n", result);
  puts("\n");
  void (*f[6]) (void) = {test_1, test_2, test_3, test_4, test_5, test_6};  
  for (int i = 0; i < 6; i++) {
    uint64_t total = 0;
    for (int j = 1; j < 1000000; j++) {      
      u = 1 << 24; //(j % 25);
      total += measure_function(f[i]);
    }
    printf("%d %lld\n", i, total / 1000000);
  }
}
