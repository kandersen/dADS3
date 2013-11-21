#!/bin/bash

for (( f = 1; f <= 9; f = f + 2))
do
    echo $f
    for ex in veb_only_heap rb_heap veb_combo_heap_100 veb_combo_heap_24 veb_combo_heap_12 veb_combo_heap_6 veb_combo_heap_3 veb_combo_heap_2 veb_combo_heap_1 bha_heap bhp_heap fh1_heap fh2_heap
    do
        echo $ex
        for (( t = 0; t <= 4; t++ ))
        do
            for (( r = 1; r <= 3; r++ ))
            do
                printf $ex"\t"$t"\t"$f"\t" >> heap_test.csv
                ./bin/$ex.out $t $f >> heap_test.csv
            done
        done
    done
done

for (( f = 10; f <= 90; f = f + 20))
do
    echo $f
    for ex in veb_only_heap rb_heap veb_combo_heap_100 veb_combo_heap_24 veb_combo_heap_12 veb_combo_heap_6 veb_combo_heap_3 veb_combo_heap_2 veb_combo_heap_1 bha_heap bhp_heap fh1_heap fh2_heap
    do
        echo $ex
        for (( t = 0; t <= 4; t++ ))
        do
            for (( r = 1; r <= 3; r++ ))
            do
                printf $ex"\t"$t"\t"$f"\t" >> heap_test.csv
                ./bin/$ex.out $t $f >> heap_test.csv
            done
        done
    done
done

for (( f = 100; f <= 900; f = f + 200))
do
    echo $f
    for ex in veb_only_heap rb_heap veb_combo_heap_100 veb_combo_heap_24 veb_combo_heap_12 veb_combo_heap_6 veb_combo_heap_3 veb_combo_heap_2 veb_combo_heap_1 bha_heap bhp_heap fh1_heap fh2_heap
    do
        echo $ex
        for (( t = 0; t <= 4; t++ ))
        do
            for (( r = 1; r <= 3; r++ ))
            do
                printf $ex"\t"$t"\t"$f"\t" >> heap_test.csv
                ./bin/$ex.out $t $f >> heap_test.csv
            done
        done
    done
done
