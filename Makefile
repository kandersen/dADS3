COMPILER = clang
FLAGS = -Wall -O3 -I src/heaps/ -I src/graph -lm
FLAGS2 = -Wall -O3 -I src2/heaps/ -I src2/graph -lm

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

XCOMMON = src2/graph/linked_graph.c src2/heaps/heap.c src2/heaps/binary_heap_array.c src2/heaps/binary_heap_pointer.c src2/heaps/fibonacci_v1.c src2/heaps/fibonacci_v2.c

tester: COMMON = src/tester.c
tester: fh1 fh2 bhp bha

versions: fh1 fh2 bhp bha

bha:	
	$(COMPILER) $(FLAGS) -o bin/bha$(NUM).out src/heaps/binary_heap_array.c $(COMMON)

bhp:
	$(COMPILER) $(FLAGS) -o bin/bhp$(NUM).out src/heaps/binary_heap_pointer.c $(COMMON)

fh1:
	$(COMPILER) $(FLAGS) -o bin/fh1$(NUM).out src/heaps/fibonacci_v1.c $(COMMON)

fh2:	
	$(COMPILER) $(FLAGS) -o bin/fh2$(NUM).out src/heaps/fibonacci_v2.c $(COMMON)

xdjk1:
	$(COMPILER) $(FLAGS2) -o bin/xdjk1.out src2/graph/dijkstra.c src2/main.c  $(XCOMMON)

xdjk2:
	$(COMPILER) $(FLAGS2) -o bin/xdjk2.out src2/graph/dijkstra_insert.c src2/main_insert.c $(XCOMMON)


creator: 
	$(COMPILER) $(FLAGS) -o bin/creator.out src/graph/linked_graph.c src/graph/creater.c $(COMMON)

test: tester
	scripts/test.sh

run:
	make d1
	make d2
	$(COMPILER) $(FLAGS) -o bin/make.out src/graph/graph.c src/graph/creater.c
	scripts/go.sh

xrun:
	make xd1
	make xd2
	$(COMPILER) $(FLAGS) -o bin/make.out src2/graph/graph.c src2/graph/creater.c
	scripts/go.sh







