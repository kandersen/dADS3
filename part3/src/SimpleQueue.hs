module SimpleQueue (SimpleQueue) where

import Queue

newtype SimpleQueue a = SQ { unSQ :: [a] }
								      deriving (Eq, Show)

instance Queue SimpleQueue where
	empty = SQ []

	pop (SQ     []) = Nothing
	pop (SQ (x:xs)) = Just (x, SQ xs)

	inject x = SQ . inject' x . unSQ
	  where
	  	inject' x     [] = [x]
	  	inject' x (y:xs) = y : inject' x xs