module StrictOkasakiQueue where

import qualified Prelude
import Control.DeepSeq

app :: ([] a1) -> ([] a1) -> [] a1
app = (Prelude.++)

plus :: Prelude.Int -> Prelude.Int -> Prelude.Int
plus = (Prelude.+)

leb :: Prelude.Int -> Prelude.Int -> Prelude.Bool
leb = (Prelude.<=)

beq_nat :: Prelude.Int -> Prelude.Int -> Prelude.Bool
beq_nat = (Prelude.==)

rev :: ([] a1) -> [] a1
rev = Prelude.reverse

data Q a = Q !Prelude.Int !([] a) !Prelude.Int !([] a) deriving (Prelude.Show)

instance NFData a => NFData (Q a) where
  rnf (Q lenh h lent t) = rnf lenh `Prelude.seq` rnf h `Prelude.seq` rnf lent `Prelude.seq` rnf t `Prelude.seq` ()

empty :: Q a1
empty =
  Q 0 [] 0 []

ltb :: Prelude.Int -> Prelude.Int -> Prelude.Bool
ltb n m =
  leb (Prelude.succ n) m

inject :: a1 -> (Q a1) -> Q a1
inject a q =
  case q of
    Q lenh h lent t -> 
      case ltb lent lenh of {
        Prelude.True -> Q lenh h (Prelude.succ lent) ((:) a t);
        Prelude.False -> Q (Prelude.succ (plus lenh lent)) (app h (rev ((:) a t))) 0 [] }
      
pop :: (Q a1) -> Q a1
pop q =
  case q of {
    Q lenh h lent t -> 
       (\fO fS n -> case n of {
           0 -> fO ();
           n -> fS ((Prelude.-) n 1) })
       (\_ ->
         case h of {
           [] -> Q 0 [] 0 [];
           (:) a l -> q})
       (\lenh' ->
         case h of {
           [] -> q;
           (:) e h' ->
             case beq_nat lenh lent of {
               Prelude.True -> Q (plus lenh' lent) (app h' (rev t)) 0 [];
               Prelude.False -> Q lenh' h' lent t}})
       lenh}


peak :: (Q a1) -> Prelude.Maybe a1
peak q =
  case q of
    Q lenh h lent t -> 
      case h of {
        [] -> Prelude.Nothing;
        (:) e l -> Prelude.Just e}

to_list :: (Q a1) -> [] a1
to_list q =
  case q of
    Q lenh h lent t -> app h (rev t)

