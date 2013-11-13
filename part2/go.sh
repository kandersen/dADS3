#!/bin/bash

for s in 100 24 12 6 3 2 1
do
clang -Wall -O3 -lm -o bin/veb_combo_tree_$s.out src/combotree/veb_tree.c src/combotree/veb_node.c src/combotree/bit_vector.c src/tests/test_tree_combo.c src/int_option.c  -I src/ -I src/trees/ -I src/combotree/ -DMIN_VEB_UNIVERSE_BITS=$s -lm
done

