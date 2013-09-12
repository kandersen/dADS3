COMMON = src/main.cpp
FLAGS = -Wall -O3 -std=c++11 -stdlib=libc++

all: clean compile

clean:
	rm -rf bin/*

compile: fh1

bha:
	clang++ $(FLAGS) -o bin/bha.out src/binary_heap_array.c $(COMMON)

bhp:
	clang++ $(FLAGS) -o bin/bhp.out src/binary_heap_pointer.c $(COMMON)

fh1:
	clang++ $(FLAGS) -o bin/fh1.out src/fibonacci_v1.cpp $(COMMON)



