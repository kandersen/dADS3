COMPILER = clang
FLAGS = -Wall -O3 -I src/heaps/ -I src/graph -lm

all: clean dijkstra

clean:
	rm -rf bin/*
	rm -f *.dot
	rm -f *.png
	rm -f *.log
	rm -f *.aux

d1: COMMON = src/main.c src/graph/linked_graph.c src/graph/dijkstra.c
d1: NUM = 1
d1: versions

d2: COMMON = src/main.c src/graph/linked_graph.c src/graph/dijkstra_insert.c
d2: NUM = 2
d2: versions

tester: COMMON = src/tester.c
tester: fh1 fh2 bhp bha

tester_ins: COMMON = src/tester.c
tester_ins: FLAGS = -Wall -O3 -DINSTRUMENT=1 -I src/heaps/ -I src/graph -lm
tester_ins: fh1 fh2 bhp bha

versions: fh1 fh2 bhp bha

bha:	
	$(COMPILER) $(FLAGS) -o bin/bha$(NUM).out src/heaps/binary_heap_array.c $(COMMON)

bhp:
	$(COMPILER) $(FLAGS) -o bin/bhp$(NUM).out src/heaps/binary_heap_pointer.c $(COMMON)

fh1:
	$(COMPILER) $(FLAGS) -o bin/fh1$(NUM).out src/heaps/fibonacci_v1.c $(COMMON)

fh2:	
	$(COMPILER) $(FLAGS) -o bin/fh2$(NUM).out src/heaps/fibonacci_v2.c $(COMMON)

creator: 
	$(COMPILER) $(FLAGS) -o bin/creator.out src/graph/linked_graph.c src/graph/creater.c $(COMMON)

test: tester
	scripts/test.sh

test_ins: tester_ins
	scripts/test.sh

run:
	make d1
	make d2
	$(COMPILER) $(FLAGS) -o bin/make.out src/graph/graph.c src/graph/creater.c
	scripts/go.sh




