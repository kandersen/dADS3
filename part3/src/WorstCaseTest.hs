module WorstCaseTest where

import Queue

import Control.Monad.State

enqueue :: Queue q => Int -> State (q Int) ()
enqueue n = modify (inject n)

dequeue :: Queue q => State (q Int) ()
dequeue = modify pop

algorithm :: Queue q => Int -> State (q Int) (q Int)
algorithm 0 = get
algorithm n = do
	forM_ [1..10] enqueue
	forM_ [1..10] $ \_ -> dequeue
	enqueue n
	algorithm (n - 1)

