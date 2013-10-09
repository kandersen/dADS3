#!/bin/bash

for (( i = 100; i <= 20000; i = i + 100 ))
do
    for p in 0 1 2 3 4 5 6 7 8 9
    do
        for ((k = 1; k <= 3; k++ ))
        do
            for ex in xdjk1
            do
                ./bin/$ex.out $i 0.$p 0 >> $ex.csv;
            done
        done
    done
    echo "size: $i"
done
