COMMON = src/main.c src/fibonacci_v1.c

all: clean compile

clean:
	rm -rf bin/*

compile:
	clang -Wall -O3 -o bin/a.out $(COMMON)

bh:
	clang -Wall -O3 -o bin/bh.out src/binary_heap.c





