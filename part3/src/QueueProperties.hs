module QueueProperties where

import Control.Monad
import Control.Applicative hiding (empty)
import Data.Function

import Test.QuickCheck

import Queue

data Action = Inject Int
            | Pop
            | Peak
            | Return (Maybe Int)
            deriving (Show)

perform :: Queue q => q Int -> [Action] -> [Maybe Int]
perform q []     = []
perform q (a:as) =
  case a of
    Inject n -> perform (inject n q) as
    Pop      -> perform (pop q) as
    Peak     -> peak q : perform q as
    Return m -> m : perform q as

actions :: (Eq a, Num a) => a -> Gen [Action]
actions n =
  oneof $ [return [],
           (:) <$> liftM Inject arbitrary <*> actions (n + 1),
           (Peak:) <$> actions n] ++
          if n == 0
          then []
          else [(Pop:) <$> actions (n - 1)]

delta :: [Action] -> Int
delta = sum . map d
  where
    d (Inject _) = 1
    d Pop        = -1
    d _          = 0 

equiv :: Queue q => q Int -> [Action] -> [Action] -> Property
equiv q c c' =
  forAll (actions 0) $ \pref ->
  forAll (actions (delta (pref ++ c))) $ \suff ->
  let
    observe x =
      perform q (pref ++ x ++ suff)
  in
   observe c == observe c'

equivFromEmpty :: Queue q => q Int -> [Action] -> [Action] -> Property
equivFromEmpty q c c' =
  forAll (actions 0) $ \suff ->
  let observe x =
        perform q (x ++ suff)
  in
   observe c == observe c'
   
prop_PeakInject q m n =
  equiv q [Inject m, Inject n, Peak] [Inject m, Peak, Inject n]
prop_InjectPop q m n =
  equiv q [Inject m, Inject n, Pop] [Inject m, Pop, Inject n]
prop_PeakEmpty q =
  equivFromEmpty q [Peak] [Return Nothing]
prop_PeakInjectEmpty q m =
  equivFromEmpty q [Inject m, Peak] [Inject m, Return (Just m)]
prop_InjectPopEmpty q m =
  equivFromEmpty q [Inject m, Pop] []

suite :: Queue q => q Int -> IO ()
suite q = do quickCheck $ prop_PeakInject q
             quickCheck $ prop_InjectPop q
             quickCheck $ prop_PeakEmpty q
             quickCheck $ prop_PeakInjectEmpty q
             quickCheck $ prop_InjectPopEmpty q

runTests :: IO ()
runTests = do test "SimpleQueue" (empty :: BasicQueue Int)
              test "SmartListQueue" (empty :: PairQueue Int)
              test "OkasakiQueue" (empty :: OkasakiQueue Int)
              test "RealTimeQueue" (empty :: RealTimeQueue Int)
  where
    test s q = do putStrLn $ "### Testing " ++ s ++ " ###"
                  suite q

main :: IO ()
main = runTests
