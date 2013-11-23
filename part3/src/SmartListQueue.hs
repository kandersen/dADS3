module SmartListQueue(SmartListQueue) where

import Queue

newtype SmartListQueue a = SLQ { unSLQ :: ([a], [a]) }
												 deriving (Eq, Show)

instance Queue SmartListQueue where
	empty = SLQ ([],[])

	inject x (SLQ (h, t)) = SLQ (h, x : t)

	pop = pop' . unSLQ
		where
			pop' ((e:h),     t) = Just (e, SLQ (h, t))
			pop' (   [], (e:t)) = Just (e, SLQ (reverse t, []))
			pop' (   [],    []) = Nothing
