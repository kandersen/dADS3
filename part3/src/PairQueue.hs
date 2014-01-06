module PairQueue where

import qualified Prelude

app :: ([] a1) -> ([] a1) -> [] a1
app = (Prelude.++)

rev :: ([] a1) -> [] a1
rev = Prelude.reverse

type Q a = (,) ([] a) ([] a)

empty :: (,) ([] a1) ([] a1)
empty =
  (,) [] []

inject :: a1 -> (Q a1) -> (,) ([] a1) ([] a1)
inject a q =
  case q of {
   (,) h t -> (,) h ((:) a t)}

pop :: (Q a1) -> (,) ([] a1) ([] a1)
pop q =
  case q of {
   (,) h t ->
    case h of {
     [] ->
      case rev t of {
       [] -> (,) [] [];
       (:) a t' -> (,) t' []};
     (:) a h' -> (,) h' t}}

peak :: (Q a1) -> Prelude.Maybe a1
peak q =
  case q of {
   (,) h t ->
    case h of {
     [] ->
      case rev t of {
       [] -> Prelude.Nothing;
       (:) x l -> Prelude.Just x};
     (:) x l -> Prelude.Just x}}

to_list :: (Q a1) -> [] a1
to_list q =
  case q of {
   (,) h t -> app h (rev t)}

