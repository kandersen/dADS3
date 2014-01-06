module BasicQueue where

import qualified Prelude

type Q a = [] a

empty :: [] a1
empty =
  []

inject :: a1 -> (Q a1) -> Q a1
inject a q =
  case q of {
   [] -> (:) a [];
   (:) x q' -> (:) x (inject a q')}

pop :: (Q a1) -> [] a1
pop q =
  case q of {
   [] -> [];
   (:) a q' -> q'}

peak :: (Q a1) -> Prelude.Maybe a1
peak q =
  case q of {
   [] -> Prelude.Nothing;
   (:) x l -> Prelude.Just x}

to_list :: (Q a1) -> Q a1
to_list q =
  q

