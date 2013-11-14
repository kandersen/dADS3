COMPILER = clang
FLAGS = -Wall -O3 -lm
COMMON = src/int_option.c src/timer.c
COMMON_TREES = -I src/ -I src/trees/ -I src/combotree/ -DMIN_VEB_UNIVERSE_BITS=0 -lm
COMMON_HEAPS = -I src/ -I src/heaps/ -I src/trees/ src/tests/test_heap.c -DMIN_VEB_UNIVERSE_BITS=0 -lm

nothings:

# DEBUG

veb_only_debug :
	$(COMPILER) $(FLAGS) -o bin/veb_only_tree.out src/trees/veb_only.c $(COMMON) $(COMMON_TREES)


# TREES

veb_only_tree :
	$(COMPILER) $(FLAGS) -o bin/veb_only_tree.out src/trees/veb_only.c src/tests/test_tree_veb.c  $(COMMON) $(COMMON_TREES)

veb_combo_tree :
	./scripts/compile_combo.sh

rb_tree :
	$(COMPILER) $(FLAGS) -o bin/rb_tree.out src/trees/rb_tree.c src/trees/rb_search_tree.c src/tests/test_tree_rb.c $(COMMON) $(COMMON_TREES)

rb_tree_prof :
	$(COMPILER) $(FLAGS) -o bin/rb_tree.out src/trees/rb_tree.c src/trees/rb_search_tree.c src/tests/test_tree_rb.c $(COMMON) $(COMMON_TREES)

# HEAPS

veb_only_heap :

	$(COMPILER) $(FLAGS) -o bin/veb_only_heap.out src/heaps/veb_heap.c src/trees/veb_only.c $(COMMON) $(COMMON_HEAPS)

veb_combo_heap :
	./scripts/compile_combo_heap.sh

rb_heap : 
	$(COMPILER) $(FLAGS) -o bin/rb_heap.out src/heaps/rb_heap.c src/trees/rb_tree.c $(COMMON) $(COMMON_HEAPS)

bha:	
	$(COMPILER) $(FLAGS) -o bin/bha_heap.out src/heaps/binary_heap_array.c $(COMMON) $(COMMON_HEAPS)

bhp:
	$(COMPILER) $(FLAGS) -o bin/bhp_heap.out src/heaps/binary_heap_pointer.c $(COMMON) $(COMMON_HEAPS)

fh1:
	$(COMPILER) $(FLAGS) -o bin/fh1_heap.out src/heaps/fibonacci_v1.c $(COMMON) $(COMMON_HEAPS)

fh2:	
	$(COMPILER) $(FLAGS) -o bin/fh2_heap.out src/heaps/fibonacci_v2.c $(COMMON) $(COMMON_HEAPS)


tester: COMMON += src/tester.c
tester: bha bhp fh1 fh2 veb1_heap rb_heap

all: clean

clean:
	rm -rf bin/*
	rm -f *.dot
	rm -f *.png
	rm -f *.log
	rm -f *.aux

test: tester
	scripts/test.sh




