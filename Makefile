COMMON = src/main.c
FLAGS = -Wall -O3 -std=c++11

all: clean compile

clean:
	rm -rf bin/*

compile:
	clang $(FLAGS) -o bin/a.out $(COMMON)

bha:
	clang $(FLAGS) -o bin/bha.out src/binary_heap_array.c

bhp:
	clang $(FLAGS) -o bin/bhp.out src/binary_heap_pointer.c

fh1:
	clang $(FLAGS) -o bin/fh1.out src/fibonacci_v1.cpp



