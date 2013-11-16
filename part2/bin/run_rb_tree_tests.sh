printf "%s,%s,%s,%s\n" "data_structure" "test_number" "fill_rate" "time"
for test in {0..6}
do
    for ((percentage = 10; percentage <= 100; percentage = percentage + 10))
    do
	for i in {0..5}
	do
	    printf "%s,%i,%i," "$1" "$test" "$percentage"
	    ./$1 $test $percentage
	done
    done
done
