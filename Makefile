COMMON = src/graph.c src/dijkstra.c
FLAGS = -Wall -O3 -std=c++11
OSNAME := ${shell uname}

ifeq ($(OSNAME), Darwin)
	FLAGS += -stdlib=libc++
endif


all: clean compile

clean:
	rm -rf bin/*

compile: fh1

bha:
	clang++ $(FLAGS) -o bin/bha.out src/binary_heap_array.c 

bhp:
	clang++ $(FLAGS) -o bin/bhp.out src/binary_heap_pointer.c $(COMMON)

fh1:
	clang++ $(FLAGS) -o bin/fh1.out src/main.cpp src/fibonacci_v1.cpp $(COMMON)

fh2:
	clang++ $(FLAGS) -o bin/fh2.out src/fibonacci_v2.c $(COMMON)

graph:
	clang++ $(FLAGS) -o bin/graph.out src/graph.c src/main_gen_graph.c	

dijkstra_bhp:
	clang++ $(FLAGS) -o bin/dijkstra.out src/binary_heap_pointer.c src/main_dijkstra.c $(COMMON)

dijkstra_fib1:
	clang++ $(FLAGS) -o bin/dijkstra.out src/fibonacci_v1.cpp src/main_dijkstra.c $(COMMON)

dijkstra_fib2:
	clang++ $(FLAGS) -o bin/dijkstra.out src/fibonacci_v2.c src/main_dijkstra.c $(COMMON)

dc:
	rm -f *.dot
	rm -f *.png

texclean: 
	rm -f *.log
	rm -f *.aux




