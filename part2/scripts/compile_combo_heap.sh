#!/bin/bash

for s in 100 24 12 6 3 2 1
do
    clang -Wall -O3 -lm -o bin/veb_combo_heap_$s.out src/timer.c src/combotree/veb_tree.c src/combotree/veb_node.c src/combotree/bit_vector.c src/tests/test_heap.c src/heaps/combo_heap.c src/int_option.c  -I src/ -I src/trees/ -I src/combotree/ -I src/heaps/ -DMIN_VEB_UNIVERSE_BITS=$s -lm
done
