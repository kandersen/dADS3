compile: extract
	ghc -O2 -threaded -rtsopts QueuePerformance.hs

run-tests: extract
	runhaskell QueueProperties.hs

run-benchmarks: extract compile
	./QueuePerformance -o finalfinalfinalnolist10012014.html -u finalfinalfinalnolist10012014.csv -s 5 +RTS -N -K2G -RTS

compile_and_extract:
	coqc Queue.v

extract: BasicQueue.hs PairQueue.hs OkasakiQueue.hs RealTimeQueue.hs

BasicQueue.hs: compile_and_extract

PairQueue.hs: compile_and_extract

OkasakiQueue.hs: compile_and_extract

RealTimeQueue.hs: compile_and_extract


clean:
	rm *.glob *.vo *.hi *.o
	rm BasicQueue.hs PairQueue.hs OkasakiQueue.hs RealTimeQueue.hs

clean-completely: clean
	rm QueuePerformance
