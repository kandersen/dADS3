{-# LANGUAGE FlexibleInstances #-}
module Main where

import Queue
import QueueSort

import Criterion.Main
import Test.QuickCheck
import Test.QuickCheck.Gen
import Control.DeepSeq

import Control.Monad.State
import Control.Applicative hiding (empty)

import Data.List
import System.Random

queueOfLength :: (Queue q, Integral a) => Int -> Gen (q a)
queueOfLength n = do
  elems <- vectorOf n arbitrarySizedIntegral
  return . fromList $ elems
  where
    fromList = foldr inject empty 

generate :: Gen a -> IO a
generate g = do
  std <- getStdGen
  return $ unGen g std 999999

basicInsertSetup :: (Queue q) => Int -> Int -> IO (q Int, [Int])
basicInsertSetup x n = do
  q <- generate $ queueOfLength x
  elems <- generate $ vectorOf n arbitrarySizedIntegral
  return (q, elems)

basicInsert :: (NFData (q Int), Queue q) => Int -> Int -> IO (q Int)
basicInsert x n = do
  (q, elems) <- basicInsertSetup x n
  (rnf q, rnf elems) `seq` (return $ foldl' (flip inject) q elems)

basicDeleteSetup :: Queue q => Int -> IO (q Int)
basicDeleteSetup = generate . queueOfLength

basicDelete :: (NFData (q Int), Queue q) => Int -> Int -> IO (q Int)
basicDelete x n = do
  q <- basicDeleteSetup x
  (rnf q) `seq` (return $ iterate pop q !! n)

                                                                          
generateInsertionTests :: (NFData (q Int), Queue q) => q Int -> [(Int, Int)] -> [(IO (q Int, [Int]), IO (q Int), Int, Int)]
generateInsertionTests q = map (\(x, n) -> (basicInsertSetup x n, basicInsert x n, x, n))

generateInsertionBenchmarks :: (NFData (q Int), Queue q) => q Int -> [Benchmark]
generateInsertionBenchmarks q =
  map benches (generateInsertionTests q basicBenchmarkSizes)
  where
    benches (setup, test, size, elems) = bgroup ("Insert " ++ show elems ++ " into queue of size " ++ show size) $
                                         [bench "setup" $ nfIO setup, bench "test" $ nfIO test]

generateDeletionTests :: (NFData (q Int), Queue q) => q Int -> [(Int, Int)] -> [(IO (q Int), IO (q Int), Int, Int)]
generateDeletionTests q = map (\(x, n) -> (basicDeleteSetup x, basicDelete x n, x, n))

generateDeletionBenchmarks :: (NFData (q Int), Queue q) => q Int -> [Benchmark]
generateDeletionBenchmarks q =
  map benches (generateDeletionTests q basicBenchmarkSizes)
  where
    benches (setup, test, size, elems) =
      bgroup ("Pop " ++ show elems ++ " from queue of size " ++ show size) $
        [bench "setup" $ nfIO setup, bench "test" $ nfIO test]

queueSortSetup :: Queue q => q Int -> Int -> IO [q Int]
queueSortSetup q n = generate $ vectorOf n (queueOfLength n)

queueSortTest :: (NFData (q Int), Queue q) => q Int -> Int -> IO [Int]
queueSortTest q n = do
  qs <- queueSortSetup q n
  (rnf qs) `seq` (return $ queueSort qs)

generateQueueSortTests q = map (\n -> (queueSortSetup q n, queueSortTest q n, n)) queueSortSizes

generateQueueSortBenchmarks q = 
  map benches (generateQueueSortTests q)
  where
    benches (setup, test, n) = 
      bgroup ("Run QueueSort with " ++ show n ++ " queues of " ++ show n ++ " elements") $
        [bench "setup" $ nfIO setup, bench "test" $ nfIO test]

enqueue :: Queue q => a -> State (q a) ()
enqueue = modify . inject 

dequeue :: Queue q => State (q a) ()
dequeue = modify pop

worstCase :: Queue q => Int -> State (q Int) (q Int)
worstCase 0 = get
worstCase n = do
  forM_ [1..10] enqueue
  forM_ [1..10] $ \_ -> dequeue
  enqueue n
  worstCase (n - 1)      

worstCaseSetup :: (Queue q) => q Int -> Int -> IO (q Int)
worstCaseSetup q = generate . queueOfLength

worstCaseTest q n = do
  q <- worstCaseSetup q n
  (rnf q) `seq` (return $ execState (worstCase (n + n)) q)

generateWorstCaseTests q = map (\n -> (worstCaseSetup q n, worstCaseTest q n, n)) worstCaseSizes

generateWorstCaseBenchmarks q = 
  map benches (generateWorstCaseTests q)
  where
    benches (setup, test, n) = 
      bgroup ("Run worst-case algorithm with queue of size " ++ show n) $
        [bench "setup" $ nfIO setup, bench "test" $ nfIO test]

data BT = Leaf
        | Node BT Int BT
        deriving (Show)

instance NFData BT where
  rnf Leaf = ()
  rnf (Node l n r) = rnf l `seq` rnf n `seq` rnf r `seq` ()

btOfDepth :: Int -> Gen BT
btOfDepth 0 = return Leaf
btOfDepth n = do
  left <- btOfDepth (n - 1)
  right <- btOfDepth (n - 1) 
  a <- arbitrarySizedIntegral
  return $ Node left a right

bft :: (Queue q) => [Int] -> State (q BT) [Int]
bft acc = do
  top <- peak <$> get
  case top of
    Nothing -> return acc
    Just bt -> case bt of
      Leaf -> dequeue >> bft acc
      (Node l a r) -> do
        dequeue
        enqueue l
        enqueue r
        bft (a : acc)

bftSetup :: Int -> IO BT
bftSetup = generate . btOfDepth

bftTest :: (Queue q) => q BT -> Int -> IO ([Int])
bftTest q n = do
  bt <- bftSetup n
  return $ evalState (bft [1]) (inject bt q)

generateBftTests q = 
  map (\n -> (bftSetup n, bftTest q n, n)) bftSizes

generateBftBenchmarks q = 
  map benches (generateBftTests q)
  where
    benches (setup, test, n) = 
      bgroup ("Traverse a balanced tree of depth " ++ show n) $
        [bench "setup" $ nfIO setup, bench "test" $ nfIO test]

powersOf2 = iterate (*2) 1
basicBenchmarkSizes = zip (take 8 $ drop 15 powersOf2) [100,100..] --, 10000, 50000] [100,100..] --, 100000, 200000, 1000000] [100,100..]
bftSizes = [] --take 1 $ drop 5 $ powersOf2
worstCaseSizes = take 4 . drop 12 $ powersOf2 --[60,120..4000] -- [100,200..1000] ++ [2000]
queueSortSizes = [] -- take 4 . drop 8 $ powersOf2 -- [3,6..160] -- [20,40..100] -- [20,40..200] --[100,200..1000]

generateBenchmarkSuite name q bftq =
  bgroup name [
    bgroup "Inserts"   $ generateInsertionBenchmarks q,
    bgroup "Deletes"   $ generateDeletionBenchmarks  q,
    bgroup "BST"       $ generateBftBenchmarks       bftq,
    bgroup "WorstCase" $ generateWorstCaseBenchmarks q,
    bgroup "QueueSort" $ generateQueueSortBenchmarks q
  ]

main :: IO ()
main = defaultMain [
--  generateBenchmarkSuite "BasicQueue"         (empty :: BasicQueue         Int) (empty :: BasicQueue         BT),
  generateBenchmarkSuite "PairQueue"          (empty :: PairQueue          Int) (empty :: PairQueue          BT),
  generateBenchmarkSuite "OkasakiQueue"       (empty :: OkasakiQueue       Int) (empty :: OkasakiQueue       BT),
  generateBenchmarkSuite "RealTimeQueue"      (empty :: RealTimeQueue      Int) (empty :: RealTimeQueue      BT),
  generateBenchmarkSuite "StrictPairQueue"    (empty :: StrictPairQueue    Int) (empty :: StrictPairQueue    BT),
  generateBenchmarkSuite "StrictOkasakiQueue" (empty :: StrictOkasakiQueue Int) (empty :: StrictOkasakiQueue BT)

  ]

