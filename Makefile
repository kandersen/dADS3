COMMON = src/main.c

all: clean compile

clean:
	rm -rf bin/*

compile:
	clang -Wall -O3 -o bin/a $(COMMON)





