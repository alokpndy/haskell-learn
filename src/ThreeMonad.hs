{-# LANGUAGE InstanceSigs #-}

module RealWorld where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Functor
import           Data.Monoid


import           System.Random



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



fib :: Int -> Int -> Writer String Int
fib a b = do
    tell ( "Adding " ++ show a ++ " to " ++ show b ++ ". ")
    return (a+ b)

main = do
    let a = runWriter $ fib 10 20 >>= fib 30 >>= fib 40
    print (fst a)



-- State Monad
-- Write as well as read
-- s == state , a == result     state -> (result, modified state)
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
    pure a = Distaste (\s -> (a,s))

    mf <*> xs = Distaste $ \s0 -> let (f, s1) = runDistaste mf s0
                                      (x, s2) = runDistaste xs s1
                                  in  (f x, s2)
--
instance Monad (Distaste s) where
  return a              =  Distaste (\s -> (a,s))

  Distaste mf >>= mg         =  Distaste (\s -> let (r,s1) = mf s
                                                    Distaste c2 = mg r in c2 s1)

-- State eg
fizzBuzz :: Integer -> String
fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
           | n`mod`5 == 0 = "Fizz"
           | n`mod`3 == 0 = "Buzz"
           | otherwise = show n
addResult :: Integer -> State [String] ()
addResult n = do
        xs <- get
        let result = fizzBuzz n
        put (result : xs)

fizzbuzzList :: [Integer] -> [String]
fizzbuzzList list =
        execState (mapM_ addResult list) []

train :: IO ()
train =
    mapM_ putStrLn $ reverse $ fizzbuzzList [1..10]



foo :: Int -> State [String] ()
foo n = do
    st <- get
    let b | n > 5 = "High"
          | n < 5 = "Low"
          | otherwise = "Equal"
    put (b : st)


bar :: Int -> State [String] ()
bar n = do
    st <- get
    let b | n > 5 = "High"
          | n < 5 = "Low"
          | otherwise = "Equal"
    put ([b])

-- execState (mapM_ breeze [2,7,11,3,5]) [] -- ["Equal","Low","High","High","Low"]
-- runState (mapM_ breeze [2,7,11,3,5]) []--
-- evalState :: State s a -> s -> a
--  ((),["Equal","Low","High","High","Low"])
--
-- :t sequenceA $ fmap breeze [3,6]       (mapM == traverse)

-- also -- execState (sequenceA $ concat $ (:) <$>  [foo 2] <*>  [[foo 8]] ) []
-- as
--     instance Traversable [] where
--        traverse f = List.foldr cons_f (pure [])
--            where cons_f x ys = (:) <$> f x <*> ys


more :: State () [Int]
more = traverse pure [1..4]

showMore = print (evalState more ())
