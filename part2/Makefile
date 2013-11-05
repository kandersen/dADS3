COMPILER = clang
FLAGS = -Wall -O3 -I src/ -I -lm
COMMON = 

veb1 :
	$(COMPILER) $(FLAGS) -o bin/veb1.out src/veb.c $(COMMON)

all: clean

clean:
	rm -rf bin/*
	rm -f *.dot
	rm -f *.png
	rm -f *.log
	rm -f *.aux


