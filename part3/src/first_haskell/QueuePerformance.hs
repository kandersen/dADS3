module QueuePerformance where

import Queue
import SimpleQueue
import SmartListQueue
import StrictConstantQueue

import Criterion.Main


data BTree = Leaf Int
           | Node BTree Int BTree

bft :: Queue q => BTree -> q BTree -> [Int]
bft = 
