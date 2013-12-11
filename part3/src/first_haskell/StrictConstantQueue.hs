module StrictConstantQueue where

import Queue

type QRep a = (Int, Int, [a], [a], [a], [a], [a], [a])

newtype StrictConstantQueue a = SCQ { unSCQ :: QRep a }

instance Queue StrictConstantQueue where
  empty = SCQ $ (0, 0, [], [], [], [], [], [])

  inject x (SCQ (p, s, f, a, b, c, d, e)) =
    SCQ . adjust $ (p, s, f, a, b, c, d, (x:e))
    where
      adjust = fix . fix . fix

  pop q@(SCQ (_, _, [], _, _, _, _, _)) = q
  pop q@(SCQ q'@(p, s, (x:f), a, b, c, d, e)) = case SCQ . fix $ q' of
    q@(SCQ (p, p', _, _, _, _, _, _)) | p == p' -> pop q
    _ -> SCQ . adjust $ (p + 1, s, f, a, b, c, d, e)
    where
      adjust = fix . fix . fix . fix
  
  peak (SCQ (p, p',    [], _, [], [], (x:_), _)) | p == p' = Just x
  peak (SCQ (_,  _, (x:_), _,  _,  _,     _, _))           = Just x
  peak                                         _           = Nothing
  
fix :: QRep a -> QRep a
fix (p, 0, f, [], b, (x:c), d, e) = (p, 0, f, [], b, c, (x:d), e)
fix (p, s, f, a, (x:b), [], d, e) = (p, s + 1, f, (x:a), b, [], d, e)
fix (p, s, f, (x:a), [], [], d, e) | s > p = (p, s - 1, f, a, [], [], (x:d), e)
fix (p, s, f, a, [], [], d, e) | s == p = (0, 0, d, [], d, e, [], [])

