module Parallel where

import           Control.Parallel.Strategies
import           Data.Tuple

x = 1 + 1 :: Int
y = x + 1


-- :sprint
-- seq a b   will evaluate fst arg to weak head normal form
--   i.e it only evaluate it to very first constructor

z = swap (x,x+1)


main = runEval $ do
    a <- rpar (sum [1..10000000])
    b <- rseq (sum [1..10000])
    return (a,b) -- returns immediately while task proceed in back

main2 = runEval $ do
    a <- rpar (sum [1..10000000])
    b <- rseq (sum [1..10000])
    return (a,b) -- waits for b then return while a proceed in back


main3 = runEval $ do
    a <- rpar (sum [1..10000000])
    b <- rseq (sum [1..10000])
    rseq a
    return (a,b)

main4 = runEval $ do
    a <- rpar (sum [1..10000000])
    b <- rpar (sum [1..10000])
    rseq a
    rseq b
    return (a,b)
