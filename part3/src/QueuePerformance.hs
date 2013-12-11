module Main where

import Queue

import Criterion.Main


data BTree = Leaf Int
           | Node BTree Int BTree

main :: IO ()
main = defaultMain []
