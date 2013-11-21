#!/bin/bash

for (( f = 1; f <= 9; f = f + 2))
do
    echo $f
    for ex in veb_only_tree rb_tree veb_combo_tree_100 veb_combo_tree_24 veb_combo_tree_12 veb_combo_tree_6 veb_combo_tree_3 veb_combo_tree_2 veb_combo_tree_1
    do
        echo $ex
        for (( t = 0; t <= 0; t++ ))
        do
            for (( r = 1; r <= 10; r++ ))
            do
                printf $ex"\t"$t"\t"$f"\t" >> tree_test.csv
                ./bin/$ex.out $t $f >> tree_test.csv
            done
        done
    done
done

for (( f = 10; f <= 90; f = f + 20))
do
    echo $f
    for ex in veb_only_tree rb_tree veb_combo_tree_100 veb_combo_tree_24 veb_combo_tree_12 veb_combo_tree_6 veb_combo_tree_3 veb_combo_tree_2 veb_combo_tree_1
    do
        echo $ex
        for (( t = 0; t <= 0; t++ ))
        do
            for (( r = 1; r <= 10; r++ ))
            do
                printf $ex"\t"$t"\t"$f"\t" >> tree_test.csv
                ./bin/$ex.out $t $f >> tree_test.csv
            done
        done
    done
done

for (( f = 100; f <= 900; f = f + 200))
do
    echo $f
    for ex in veb_only_tree rb_tree veb_combo_tree_100 veb_combo_tree_24 veb_combo_tree_12 veb_combo_tree_6 veb_combo_tree_3 veb_combo_tree_2 veb_combo_tree_1
    do
        echo $ex
        for (( t = 0; t <= 0; t++ ))
        do
            for (( r = 1; r <= 10; r++ ))
            do
                printf $ex"\t"$t"\t"$f"\t" >> tree_test.csv
                ./bin/$ex.out $t $f >> tree_test.csv
            done
        done
    done
done
