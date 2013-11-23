module Splitter where

import Data.List

readInput = fmap (map words . lines) $ readFile "tree_test.csv"

compareTest [_,a,_,_] [_,b,_,_] = compare a b

sameTest [_,a,_,_] [_,b,_,_] = a == b

removeTest [alg, _, fill, time] = [alg, fill, time]

processAndSplit = map (map removeTest) . groupBy sameTest . sortBy compareTest

writeFiles _     [] = return ()
writeFiles n (f:fs) = do writeFile ("tree_test_" ++ show n ++ ".csv") (unlines . map unwords $ f)
                         writeFiles (n + 1) fs

main = fmap processAndSplit readInput >>= writeFiles 0
