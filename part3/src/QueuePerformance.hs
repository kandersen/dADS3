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
  (rnf elems) `seq` (return $ foldl' (flip inject) q elems)

basicDeleteSetup :: Queue q => Int -> IO (q Int)
basicDeleteSetup = generate . queueOfLength

basicDelete :: (NFData (q Int), Queue q) => Int -> Int -> IO (q Int)
basicDelete x n = do
  q <- basicDeleteSetup x
  return $ iterate pop q !! n

                                                                          
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
  return $ queueSort qs

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
  return $ execState (worstCase (n + n)) q

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
  (rnf bt) `seq` (return $ evalState (bft [1]) (inject bt q))

generateBftTests q = 
  map (\n -> (bftSetup n, bftTest q n, n)) bftSizes
 
generateBftBenchmarks q = 
  map benches (generateBftTests q)
  where
    benches (setup, test, n) = 
      bgroup ("Traverse a balanced tree of depth " ++ show n) $
        [bench "setup" $ nfIO setup, bench "test" $ nfIO test]

simpleSetup :: Int -> IO [Int]
simpleSetup n = generate $ vectorOf n arbitrarySizedIntegral

simpleTest :: (Queue q) => q Int -> Int -> IO (q Int)
simpleTest q n = do
  elems <- simpleSetup n
  (rnf elems) `seq` return $ iterate pop (foldl (flip inject) q elems) !! n
  
generateSimpleTests q =
  map (\n -> (simpleSetup n, simpleTest q n, n)) simpleSizes

generateSimpleBenchmarks q = 
  map benches (generateSimpleTests q)
  where
    benches (setup, test, n) = 
      bgroup ("Insert " ++ show n ++ " followed by as many deletes") $
        [bench "setup" $ nfIO setup, bench "test" $ nfIO test]

generateBenchmarkSuite name q bftq =
  bgroup name [
    bgroup "InsertsDeletes" $ generateSimpleBenchmarks    q,
    bgroup "Inserts"        $ generateInsertionBenchmarks q,
    bgroup "BST"            $ generateBftBenchmarks       bftq,
    bgroup "WorstCase"      $ generateWorstCaseBenchmarks q,
    bgroup "QueueSort"      $ generateQueueSortBenchmarks q
  ]

powersOf2           = iterate (*2) 1
simpleSizes         = take 2 powersOf2
basicBenchmarkSizes = zip (take 22 powersOf2) [0,0..]
bftSizes            = [1,2,4,6,8,10,12,14,16,18]
worstCaseSizes      = take 12 powersOf2
queueSortSizes      = [1, 2, 4, 8, 16, 32, 64, 128, 96, 128, 160, 192, 224, 256]

main :: IO ()
main = defaultMain [
--generateBenchmarkSuite "BasicQueue"         (empty :: BasicQueue         Int) (empty :: BasicQueue         BT),
  generateBenchmarkSuite "PairQueue"          (empty :: PairQueue          Int) (empty :: PairQueue          BT),
  generateBenchmarkSuite "OkasakiQueue"       (empty :: OkasakiQueue       Int) (empty :: OkasakiQueue       BT),
  generateBenchmarkSuite "RealTimeQueue"      (empty :: RealTimeQueue      Int) (empty :: RealTimeQueue      BT),
  generateBenchmarkSuite "StrictPairQueue"    (empty :: StrictPairQueue    Int) (empty :: StrictPairQueue    BT),
  generateBenchmarkSuite "StrictOkasakiQueue" (empty :: StrictOkasakiQueue Int) (empty :: StrictOkasakiQueue BT)
  ]

