COMPILER = clang
FLAGS = -Wall -O3 -I src/heaps/

all: clean tester

clean:
	rm -rf bin/*

dijkstra: COMMON = lame 
dijkstra: versions

tester: COMMON = src/tester.c
tester: versions

versions: fh1 fh2 bhp

bha:
	$(COMPILER) $(FLAGS) -o bin/bha.out src/heaps/binary_heap_array.c $(COMMON)

bhp:
	$(COMPILER) $(FLAGS) -o bin/bhp.out src/heaps/binary_heap_pointer.c $(COMMON)

fh1:
	$(COMPILER) $(FLAGS) -o bin/fh1.out src/heaps/fibonacci_v1.c $(COMMON)

fh2:
	$(COMPILER) $(FLAGS) -o bin/fh2.out src/heaps/fibonacci_v2.c $(COMMON)

graph:
	$(COMPILER) $(FLAGS) -o bin/graph.out src/graph.c src/main_gen_graph.c	

dc:
	rm -f *.dot
	rm -f *.png

texclean: 
	rm -f *.log
	rm -f *.aux




