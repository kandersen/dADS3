#!/bin/bash

for (( i = 100; i <= 200000; i = i + 100 ))
do
    for ex in fh1 fh2 bhp
    do
        for (( t = 0; t <= 7; t++ ))
        do
            for (( r = 1; r <= 3; r++ ))
            do
                printf $i"\t"$t"\t"$r"\t" >> $ex-$t.csv
                ./bin/$ex.out $t $i >> $ex-$t.csv
            done
        done
    done
done


