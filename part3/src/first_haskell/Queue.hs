module Queue where

import Data.Maybe

class Queue q where
  empty  :: q a
  inject :: a -> q a -> q a  
  pop    :: q a -> q a
  peak   :: q a -> Maybe a
