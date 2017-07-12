module Parallel where

import           Control.Exception
import           Control.Parallel
import           Control.Parallel.Strategies
import           Data.Time.Clock
import           Data.Tuple
import           System.Environment
import           Text.Printf

x = 1 + 1 :: Int
y = x + 1


-- :sprint
-- seq a b   will evaluate fst arg to weak head normal form
--   i.e it only evaluate it to very first constructor

z = swap (x,x+1)

-- to profile open Terminal at this location and in there
-- ghc parallel.hs -O2 && time ./parallel
-- :set +s in repl

-- to test 2 thread
-- stack parallel.hs -O2 -threaded  ./parallel +RTS-N2
{--
time: 47.73s
(24157817,14930352)
time: 125.21s
--}

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

main = do

    let test = test2
    t0 <- getCurrentTime
    r <- evaluate (runEval test2)
    printTimeSince t0
    print r
    printTimeSince t0

test2 = do
    x <- rpar (fib 36)
    y <- rseq (fib 35)
    return (x,y)

printTimeSince t0 = do
  t1 <- getCurrentTime
  printf "time: %.2fs\n" (realToFrac (diffUTCTime t1 t0) :: Double)



main1 = runEval $ do
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
