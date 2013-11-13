#!/bin/bash

for s in 100 24 12 6 3 2 1
do
    clang -O3 -I ./ -I ../ -DMIN_VEB_UNIVERSE_BITS=$s test.c ../int_option.c veb_tree.c veb_node.c bit_vector.c -o a.out
    ./a.out 0 0
done

