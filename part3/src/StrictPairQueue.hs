module StrictPairQueue where

import qualified Prelude
import Control.DeepSeq

app :: ([] a1) -> ([] a1) -> [] a1
app = (Prelude.++)

rev :: ([] a1) -> [] a1
rev = Prelude.reverse

data Q a = Q !([] a) !([] a) deriving (Prelude.Show)

instance NFData a => NFData (Q a) where
  rnf (Q h t) = rnf h `Prelude.seq` rnf t `Prelude.seq` ()

empty :: Q a1
empty =
  Q [] []

inject :: a1 -> (Q a1) -> Q a1
inject a q =
  case q of {
   Q h t -> Q h ((:) a t)}

pop :: (Q a1) -> Q a1
pop q =
  case q of {
   Q h t ->
    case h of {
     [] ->
      case rev t of {
       [] -> Q [] [];
       (:) a t' -> Q t' []};
     (:) a h' -> Q h' t}}

peak :: (Q a1) -> Prelude.Maybe a1
peak q =
  case q of {
   Q h t ->
    case h of {
     [] ->
      case rev t of {
       [] -> Prelude.Nothing;
       (:) x l -> Prelude.Just x};
     (:) x l -> Prelude.Just x}}

to_list :: (Q a1) -> [] a1
to_list q =
  case q of {
   Q h t -> app h (rev t)}

