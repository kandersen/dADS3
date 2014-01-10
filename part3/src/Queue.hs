{-# LANGUAGE FlexibleInstances, StandaloneDeriving #-}
module Queue (
  Queue(..),
  BasicQueue,
  PairQueue,
  OkasakiQueue,
  RealTimeQueue,
  StrictPairQueue,
  StrictOkasakiQueue,
  isEmpty) where

import Data.Maybe

import qualified BasicQueue
import qualified PairQueue
import qualified OkasakiQueue
import qualified RealTimeQueue
import Control.DeepSeq
import qualified StrictPairQueue
import qualified StrictOkasakiQueue


class Queue q where
  empty  :: q a
  inject :: a -> q a -> q a  
  pop    :: q a -> q a
  peak   :: q a -> Maybe a

newtype BasicQueue a = BQ { unBQ :: BasicQueue.Q a }
                     deriving (Show)

instance Queue BasicQueue where
  empty    = BQ BasicQueue.empty
  inject x = BQ . BasicQueue.inject x . unBQ
  pop      = BQ . BasicQueue.pop . unBQ
  peak     = BasicQueue.peak . unBQ

instance NFData a => NFData (BasicQueue a) where
  rnf (BQ q) = (rnf q) `seq` ()
  
newtype PairQueue a = PQ { unPQ :: PairQueue.Q a }
                    deriving (Show)

instance Queue PairQueue where
  empty    = PQ PairQueue.empty
  inject x = PQ . PairQueue.inject x . unPQ
  pop      = PQ . PairQueue.pop . unPQ
  peak     = PairQueue.peak . unPQ

instance NFData a => NFData (PairQueue a) where
  rnf (PQ q) = (rnf q) `seq` ()

newtype OkasakiQueue a = OQ { unOQ :: OkasakiQueue.Q a }
                       deriving (Show)
                                
instance Queue OkasakiQueue where
  empty    = OQ OkasakiQueue.empty
  inject x = OQ . OkasakiQueue.inject x . unOQ
  pop      = OQ . OkasakiQueue.pop . unOQ
  peak     = OkasakiQueue.peak . unOQ

instance NFData a => NFData (OkasakiQueue a) where
  rnf (OQ q) = (rnf q) `seq` ()

newtype RealTimeQueue a = RQ { unRQ :: RealTimeQueue.Q a }
                        deriving (Show)
deriving instance Show a => Show (RealTimeQueue.RotationState a)

instance Queue RealTimeQueue where
  empty    = RQ RealTimeQueue.empty
  inject x = RQ . RealTimeQueue.inject x . unRQ
  pop      = RQ . RealTimeQueue.pop . unRQ
  peak     = RealTimeQueue.peak . unRQ

instance NFData a => NFData (RealTimeQueue.RotationState a) 

instance NFData a => NFData (RealTimeQueue a) where
  rnf (RQ q) = (rnf q) `seq` ()

newtype StrictPairQueue a = SPQ { unSPQ :: StrictPairQueue.Q a }
                    deriving (Show)

instance Queue StrictPairQueue where
  empty    = SPQ StrictPairQueue.empty
  inject x = SPQ . StrictPairQueue.inject x . unSPQ
  pop      = SPQ . StrictPairQueue.pop . unSPQ
  peak     = StrictPairQueue.peak . unSPQ

instance NFData a => NFData (StrictPairQueue a) where
  rnf (SPQ q) = (rnf q) `seq` ()

newtype StrictOkasakiQueue a = SOQ { unSOQ :: StrictOkasakiQueue.Q a }
                       deriving (Show)
                                
instance Queue StrictOkasakiQueue where
  empty    = SOQ StrictOkasakiQueue.empty
  inject x = SOQ . StrictOkasakiQueue.inject x . unSOQ
  pop      = SOQ . StrictOkasakiQueue.pop . unSOQ
  peak     = StrictOkasakiQueue.peak . unSOQ

instance NFData a => NFData (StrictOkasakiQueue a) where
  rnf (SOQ q) = (rnf q) `seq` ()


isEmpty :: (Queue q) => q a -> Bool
isEmpty = isNothing . peak  
