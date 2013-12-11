extract: BasicQueue.hs PairQueue.hs OkasakiQueue.hs

compile: extract
	ghc -O2 QueuePerformance.hs

run-tests: extract
	runhaskell QueueProperties.hs

run-benchmarks: extract compile
	./QueuePerformance -o report.html

compile_and_extract:
	coqc Queue.v

BasicQueue.hs: compile_and_extract

PairQueue.hs: compile_and_extract

OkasakiQueue.hs: compile_and_extract

clean:
	rm *.glob *.vo *.hi *.o
	rm BasicQueue.hs PairQueue.hs OkasakiQueue.hs

clean-completely: clean
	rm QueuePerformance
	rm report.html
