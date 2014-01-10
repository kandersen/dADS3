module StrictOkasakiQueue where

import qualified Prelude
import Control.DeepSeq

length :: ([] a1) -> Prelude.Int
length = Prelude.length

app :: ([] a1) -> ([] a1) -> [] a1
app = (Prelude.++)

leb :: Prelude.Int -> Prelude.Int -> Prelude.Bool
leb = (Prelude.<=)

beq_nat :: Prelude.Int -> Prelude.Int -> Prelude.Bool
beq_nat = (Prelude.==)

rev :: ([] a1) -> [] a1
rev = Prelude.reverse

data Q a = Q !([] a) !([] a) deriving (Prelude.Show)

instance NFData a => NFData (Q a) where
  rnf (Q h t) = rnf h `Prelude.seq` rnf t `Prelude.seq` ()

empty :: Q a1
empty =
  Q [] []

ltb :: Prelude.Int -> Prelude.Int -> Prelude.Bool
ltb n m =
  leb (Prelude.succ n) m

inject :: a1 -> (Q a1) -> Q a1
inject a q =
  case q of {
   Q h t ->
    case ltb (length t) (length h) of {
     Prelude.True -> Q h ((:) a t);
     Prelude.False -> Q (app h (rev ((:) a t))) []}}

pop :: (Q a1) -> Q a1
pop q =
  case q of {
   Q h t ->
    case h of {
     [] -> Q [] [];
     (:) e h' ->
      case beq_nat (length h) (length t) of {
       Prelude.True -> Q (app h' (rev t)) [];
       Prelude.False -> Q h' t}}}

peak :: (Q a1) -> Prelude.Maybe a1
peak q =
  case q of {
   Q h t ->
    case h of {
     [] -> Prelude.Nothing;
     (:) e l -> Prelude.Just e}}

to_list :: (Q a1) -> [] a1
to_list q =
  case q of {
   Q h t -> app h (rev t)}

