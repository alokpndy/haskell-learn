module CovarianceAndContra where

import           Data.Functor.Contravariant

-- https://www.schoolofhaskell.com/user/commercial/content/covariance-contravariance

{-
What we just saw is that fmap takes a function from a -> b, and lifts it to f a -> f b. Notice that the a is always the "input" in both cases, whereas the b is the "output" in both cases. By contrast, mapMakeString has the normal f a -> f b, but the initial function has its types reversed: b -> a. This is the core of covariance vs contravariance:

In covariance, both the original and lifted functions point in the same direction (from a to b)

In contravariance, the original and lifted functions point in opposite directions (one goes from a to b, the other from b to a)


mapMakeString :: (b -> a) -> MakeString a -> MakeString b
fmap          :: (a -> b) -> MakeString a -> MakeString b

-}


newtype MakeString a = MakeString { makeString :: a -> String }

instance Contravariant MakeString where
    contramap f (MakeString g) = MakeString (g . f)

showInt :: MakeString Int
showInt = MakeString show

plus3ShowInt :: MakeString Int
plus3ShowInt = contramap (+ 3) showInt

main :: IO ()
main = putStrLn $ makeString plus3ShowInt 5


data Price = Integer | Float

foo :: Price -> Bool
foo p = True
