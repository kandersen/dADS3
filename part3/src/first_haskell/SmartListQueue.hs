module SmartListQueue(SmartListQueue) where

import Queue

newtype SmartListQueue a = SLQ { unSLQ :: ([a], [a]) }
                         deriving (Eq, Show)

instance Queue SmartListQueue where
  empty = SLQ ([],[])
  
  inject x (SLQ (h, t)) = SLQ (h, x : t)

  pop = pop' . unSLQ
    where
      pop' q@(   [],    []) = SLQ q
      pop'   ((_:h),     t) = SLQ (h, t)
      pop'   (   [],     t) = SLQ (tail . reverse $ t, [])

  peak = peak' . unSLQ
    where
      peak' ((e:_),       _) = Just e
      peak' (   [], t@(_:_)) = Just . head . reverse $ t
      peak'                _ = Nothing
