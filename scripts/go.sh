#!/bin/bash

for (( i = 100; i <= 20000; i = i + 100 ))
do
    for p in 0 1 2 3 4 5 6 7 8 9
    do
        for (( k = 1; k <= 3; k++ ))
        do
            seed=$((($p+1) * 10000000 + $i * 100 + 10 * $k))
            for ex in fh11 fh21 bhp1 bha1 fh12 fh22 bhp2 bha2
            do
                for (( r = 1; r <= 3; r++ ))
                do
                    printf $i"\t"$p"\t"$k"\t"$r"\t"$ex"\t" >> $ex.csv;
                    ./bin/$ex.out 0.$p $i $seed >> $ex.csv;
                done
            done
        done
    done
    echo "size: $i"
done


