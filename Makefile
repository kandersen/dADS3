COMPILER = clang
FLAGS = -Wall -O3 -I src/heaps/ -I src/graph -lm

all: clean dijkstra

clean:
	rm -rf bin/*
	rm -f *.dot
	rm -f *.png
	rm -f *.log
	rm -f *.aux

dijkstra: COMMON = src/main.c src/graph/graph.c src/graph/dijkstra.c
dijkstra: versions

tester: COMMON = src/tester.c
tester: versions

versions: fh1 fh2 bhp bha

bha:	
	$(COMPILER) $(FLAGS) -o bin/bha.out src/heaps/binary_heap_array.c $(COMMON)

bhp:
	$(COMPILER) $(FLAGS) -o bin/bhp.out src/heaps/binary_heap_pointer.c $(COMMON)

fh1:
	$(COMPILER) $(FLAGS) -o bin/fh1.out src/heaps/fibonacci_v1.c $(COMMON)

fh2:	
	$(COMPILER) $(FLAGS) -o bin/fh2.out src/heaps/fibonacci_v2.c $(COMMON)

run:	dijkstra
	$(COMPILER) $(FLAGS) -o bin/make.out src/graph/graph.c src/graph/creater.c
	scripts/go.sh






