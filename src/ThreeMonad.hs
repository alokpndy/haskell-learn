{-# LANGUAGE InstanceSigs #-}

module RealWorld where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Functor
import           Data.Monoid



{- The Reader Functor

fmap :: (a -> b) -> (r -> a) -> (r -> b)
            f          g
instance Functor ((->) r) where
      fmap f g = f . g
   or fmap     = (.)

-}

newtype Mystery r a = Mystery { solve :: r -> a }


instance Functor (Mystery r) where
    fmap f m = Mystery $ f . solve m
    -- or fmap f (Mystery ra) = Mystery $ f . ra

-- Note - fmap f x = pure f <*> x

instance Applicative (Mystery r) where
    pure :: a -> Mystery r a
    pure a = Mystery $ \ r -> a
    -- or Mystery . const

    (<*>) :: Mystery r (a -> b) -> Mystery r a -> Mystery r b
    Mystery rab <*> Mystery ra = Mystery $ \r ->  rab r (ra r)


-- Note - fmap f xs = xs >>= return . f
instance Monad (Mystery r) where
    return :: a -> Mystery r a
    return = pure

    (>>=) :: Mystery r a -> (a -> Mystery r b) -> Mystery r b
    Mystery ra >>= aRb = Mystery $ \r -> solve (aRb (ra r)) r


{- The Reader Monad
newtype Reader r a = Reader { runReader :: r -> a }
ask :: MonadReader r m => m r
-}


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

bookWorm'' :: Reader Int Int
bookWorm'' = (1 +) <$> ask


-- (10 +) . (runReader ask) $ 10
--    f   .    g





{- Writer Monad
data Writer w a = Writer {runWriter :: (a, w)}
-}

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


newtype Distaste s a = Distaste { runDistaste :: s -> (a, s) }
-- Functor
instance Functor (Distaste s) where
    fmap :: (a -> b) -> Distaste s a -> Distaste s b
    fmap f (Distaste g) = Distaste $ \ s ->  mapp f (g s)
            where mapp f (x, y) = (f x, y)

--
instance Applicative (Distaste s) where
    pure :: a -> Distaste s a
    pure a = Distaste $ \ s -> (a, s)

    (<*>) :: Distaste s (a -> b) -> Distaste s a -> Distaste s b
    Distaste f <*> Distaste g = Distaste $ \s -> mapp (fst (f s)) (g s)
                where mapp h (x, y) = (h x, y)

--
instance Monad (Distaste s) where
    return  =  pure

    (>>=) :: Distaste s a -> (a -> Distaste s b) -> Distaste s b
    Distaste f >>= g = Distaste $ \ s -> runDistaste (g (fst (f s))) s
