module Queue (
  Queue(..),
  BasicQueue,
  PairQueue,
  OkasakiQueue) where

import qualified BasicQueue
import qualified PairQueue
import qualified OkasakiQueue

class Queue q where
  empty  :: q a
  inject :: a -> q a -> q a  
  pop    :: q a -> q a
  peak   :: q a -> Maybe a

newtype BasicQueue a = BQ { unBQ :: BasicQueue.Q a }

instance Queue BasicQueue where
  empty    = BQ BasicQueue.empty
  inject x = BQ . BasicQueue.inject x . unBQ
  pop      = BQ . BasicQueue.pop . unBQ
  peak     = BasicQueue.peak . unBQ

newtype PairQueue a = PQ { unPQ :: PairQueue.Q a }

instance Queue PairQueue where
  empty    = PQ PairQueue.empty
  inject x = PQ . PairQueue.inject x . unPQ
  pop      = PQ . PairQueue.pop . unPQ
  peak     = PairQueue.peak . unPQ

newtype OkasakiQueue a = OQ { unOQ :: OkasakiQueue.Q a }

instance Queue OkasakiQueue where
  empty    = OQ OkasakiQueue.empty
  inject x = OQ . OkasakiQueue.inject x . unOQ
  pop      = OQ . OkasakiQueue.pop . unOQ
  peak     = OkasakiQueue.peak . unOQ


