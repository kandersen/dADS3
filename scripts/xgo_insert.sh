#!/bin/bash

for (( i = 100; i <= 8000; i = i + 100 ))
do
    for p in 0 1 2 3 4 5 6 7 8 9
    do
        for (( t = 0; t <= 3; t++))
        do
            for (( k = 1; k <= 3; k++ ))
            do
                seed=$((($p+1) * 10000000 + $i * 100 + 10 * $k))
                printf $i"\t"$p"\t"$t"\t"$k"\t" >> xdjk2.csv;
                ./bin/xdjk2.out $i 0.$p $t $seed >> xdjk2.csv;
            done
        done
    done
    echo "size: $i"
done
