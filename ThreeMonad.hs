module RealWorld where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Functor
import           Data.Monoid





{- The Reader Functor
since  (->) :: * -> * -> *
also in Functor f, f :: * -> *

fmap :: (a -> b) -> (r -> a) -> (r -> b)
            f          g
instance Functor ((->) r) where
      fmap f g = f . g
   or fmap f g = (.) f g
   or fmap     = (.)
-}


-- The Reader Monad

-- where, ask :: MonadReader r m => m r

circA :: Reader Int Int
circA = do
    mpI <- ask
    return (mpI * 10 * 10)

cylA :: Reader Int Int
cylA = do
    mpI <- ask
    return (mpI * 10 * 10 * 20)

addCAndCl :: Reader Int Int
addCAndCl = do
    cA <- circA
    lA <- cylA
    return (cA + lA)

--------------------
bookWorm :: Reader Int Int
bookWorm = do
    val <- ask
    return $ (+) 1 val

--
bookWorm' :: Reader Int Int
bookWorm' = fmap (1 +) ask
--          fmap f g = (.) f g = (.)

-- (10 +) . (runReader ask) $ 10
--    f   .    g





-- Writer Monad
-- Rteurn Log
-- data Writer w a = Writer {rinWriter :: (a, w)}
-- IO of what is going on to the arg in every step
half :: Int -> Writer String Int
half x = do
    tell ("I just halved " ++ show x ++ "! ")
    return (x `div` 2)

ho = runWriter $ half 200 >>= half >>= half

-- State Monad
-- Write as well as read
-- data State s a = State {runState :: s -> (a, s)}
-- return a = State $ \s -> (a,s)
greeter :: State String String
greeter = do
    name <- get
    put "Pandey"
    return ("Hello " ++ name ++ "!")

gee = runState greeter "Alok"
