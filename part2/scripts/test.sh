#!/bin/bash

for (( u = 2048; u <= 2^12; u = u * 2 * 2))
do
    for (( i = 1000; i < u; i = i + 1000 ))
    do
    echo "size: $i"
        echo "universe: $u"
        for ex in fh1 fh2 bhp bha veb1_heap
        do
            for (( t = 0; t <= 6; t++ ))
            do
                echo "test-nr: $t"
                for (( r = 1; r <= 3; r++ ))
                do
                    printf $i"\t"$u"\t"$t"\t"$r"\t"0"\t" >> $ex-$t.csv
                    ./bin/$ex.out $t $i $u 0 >> $ex-$t.csv
                    printf $i"\t"$u"\t"$t"\t"$r"\t"1"\t" >> $ex-$t.csv
                    ./bin/$ex.out $t $i $u 1 >> $ex-$t.csv
                done
            done
        done
    done
done

