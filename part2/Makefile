COMPILER = clang
FLAGS = -Wall -O3 -lm
COMMON = src/int_option.c
COMMON_TREES = -I src/trees/ -lm
COMMON_HEAPS = -I src/heaps/ -I src/trees/  -lm

nothing:

tester: COMMON += src/tester.c
tester: bha bhp fh1 fh2 veb1_heap rb_heap

veb1_heap : 
	$(COMPILER) $(FLAGS) -o bin/veb1_heap.out src/heaps/veb.c $(COMMON) $(COMMON_HEAPS)

rb_heap : 
	$(COMPILER) $(FLAGS) -o bin/rb_heap.out src/heaps/rb_heap.c src/trees/rb_tree.c $(COMMON) $(COMMON_HEAPS)

bha:	
	$(COMPILER) $(FLAGS) -o bin/bha$(NUM).out src/heaps/binary_heap_array.c $(COMMON) $(COMMON_HEAPS)

bhp:
	$(COMPILER) $(FLAGS) -o bin/bhp$(NUM).out src/heaps/binary_heap_pointer.c $(COMMON) $(COMMON_HEAPS)

fh1:
	$(COMPILER) $(FLAGS) -o bin/fh1$(NUM).out src/heaps/fibonacci_v1.c $(COMMON) $(COMMON_HEAPS)

fh2:	
	$(COMPILER) $(FLAGS) -o bin/fh2$(NUM).out src/heaps/fibonacci_v2.c $(COMMON) $(COMMON_HEAPS)

all: clean

clean:
	rm -rf bin/*
	rm -f *.dot
	rm -f *.png
	rm -f *.log
	rm -f *.aux

test: tester
	scripts/test.sh




