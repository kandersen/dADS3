COMMON = src/main.c src/fibonacci_v1.c

all: clean compile

clean:
	rm -rf bin/*

compile:
	clang -Wall -O3 -o bin/a.out $(COMMON)

bha:
	clang -Wall -O3 -o bin/bha.out src/binary_heap_array.c

bhp:
	clang -Wall -O3 -o bin/bhp.out src/binary_heap_pointer.c




