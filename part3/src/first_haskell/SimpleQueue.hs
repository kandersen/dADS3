module SimpleQueue (SimpleQueue) where

import Queue

newtype SimpleQueue a = SQ { unSQ :: [a] }
                      deriving (Eq, Show)

instance Queue SimpleQueue where
  empty = SQ []
  
  pop q@(SQ     []) = q
  pop   (SQ (_:xs)) = SQ xs

  inject x = SQ . inject' x . unSQ
    where
      inject' x     [] = [x]
      inject' x (y:xs) = y : inject' x xs

  peak (SQ (x:_)) = Just x
  peak          _ = Nothing
