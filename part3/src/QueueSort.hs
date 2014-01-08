{-# LANGUAGE FlexibleInstances,
             StandaloneDeriving,
             TupleSections #-}
module QueueSort where

import Queue
import Data.Maybe
import Data.List

findMin :: (Queue q, Ord a) => [(q a, Int)] -> (a, q a, Int)
findMin = (\(mmin, q, n) -> (fromJust mmin, q, n)) . foldl step acc
  where
    acc = (Nothing, empty, 0)
    step (curMin, tail, index) (q, qsIndex) =
      case peak q of
        Nothing ->               (curMin, tail   , index)
        Just m  -> case curMin of
          Nothing ->             (peak q, (pop q), qsIndex)
          Just m' | m < m' ->    (peak q, (pop q), qsIndex)
                  | otherwise -> (curMin, tail   , index   )

step :: (Queue q, Ord a) => [q a] -> ([q a], a)
step qs = (map (fst . modify) taggedQs, next)
  where
    taggedQs = qs `zip` [0..]
    (next, q', index) = findMin $ taggedQs
    modify (p@(q, n)) | n == index = (q', n)
                      | otherwise  = p

done :: (Queue q) => [q a] -> Bool
done = all isEmpty

iter :: (Queue q, Ord a) => ([q a], [a]) -> ([q a], [a])
iter (qs, acc) = if done qs
                 then (qs, acc)
                 else let (qs', next) = step qs in
                 iter (qs', acc ++ [next])

queueSort :: (Queue q, Ord a) => [q a] -> [a]
queueSort = snd . iter . (,[])