#!/bin/bash

for (( i = 50; i <= 20000; i = i + 50 ))
do
    for p in 0 1 2 3 4 5 6 7 8 9
    do
        for (( k = 1; k <= 5; k++ ))
        do
            ./bin/make.out $i 0.$p > tmp.gra

            for ex in fh1 fh2 bhp
            do
                for (( r = 1; r <= 5; r++ ))
                do
                    printf $i"\t"$p"\t"$k"\t"$r"\t"$ex"\t" >> result.txt;
                    ./bin/$ex.out tmp.gra $i >> result.txt;
                done
            done
        done
    done
done


