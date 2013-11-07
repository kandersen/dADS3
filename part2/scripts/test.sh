#!/bin/bash

for (( i = 1000; i <= 600000; i = i + 1000 ))
do
    for ex in fh1 fh2 bhp bha veb1_heap
    do
        for (( t = 0; t <= 5; t++ ))
        do
            echo "test-nr: $t"
            for (( r = 1; r <= 3; r++ ))
            do
                printf $i"\t"$t"\t"$r"\t" >> $ex-$t.csv
                ./bin/$ex.out $t $i >> $ex-$t.csv
            done
        done
    done
    echo "all size: $i"
done
