COMMON = src/main.cpp

all: clean compile

clean:
	rm -rf bin/*

compile:
	clang -wall -O3 -o bin/a $(COMMON)





