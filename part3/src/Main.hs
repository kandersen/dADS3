module Main where

import Queue
import SimpleQueue
import SmartListQueue

import Criterion.Main

test :: Queue q => q Int
test = inject 1 empty

foo :: Queue q => q Int -> Int
foo q = case pop q of
	Nothing -> 3
	Just (x, q') -> x + 41

insertions :: Queue q => q Int -> Int -> q Int
insertions q 0 = q
insertions q n = inject n $ insertions q (n - 1)

main :: IO ()
main = defaultMain [
		bgroup "insertions" [
			bench "simple insert 100000" $ whnf (insertions (empty :: SimpleQueue Int)) 100000,
			bench "smart insert 100000" $ whnf (insertions (empty :: SmartListQueue Int)) 100000
		]
	]