#!/bin/bash

for (( f = 1; u <= 9; u = u + 2))
do
    echo $f
    for ex in veb_only_tree rb_tree veb_combo_tree_100 veb_combo_tree_24 veb_combo_tree_12 veb_combo_tree_6 veb_combo_tree_3 veb_combo_tree_2 veb_combo_tree_1
    do
        echo $ex
        for (( t = 0; t <= 6; t++ ))
        do
            for (( r = 1; r <= 3; r++ ))
            do
                printf $ex"\t"$t"\t"$f"\t" >> tree_test.csv
                ./bin/$ex.out $t $f >> tree_test.csv
            done
        done
    done
done
