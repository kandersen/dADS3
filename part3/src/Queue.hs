module Queue where

class Queue q where
	empty  :: q a
	inject :: a -> q a -> q a
	pop    :: q a -> Maybe (a, q a)
