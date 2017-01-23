{-# LANGUAGE Arrows #-}

module Arrows where

import           Control.Arrow
import           Control.Category
import           Control.Monad
import           Prelude          hiding (id, (.))

-- Arrow


newtype SimpleFunc a b = SimpleFunc { runF :: a -> b }

instance Arrow SimpleFunc where
    arr f = SimpleFunc f
    first (SimpleFunc f) = SimpleFunc (mapFst f)
                    where mapFst g (a,b) = (g a, b)
    second (SimpleFunc f) = SimpleFunc (mapSnd f)
                        where mapSnd g (a,b) = (a, g b)

instance Category SimpleFunc where
    (SimpleFunc g) . (SimpleFunc f) = SimpleFunc (g . f)
    id = arr id

-- Arrow operation :: Split into dupplicate
split :: (Arrow a) => a b (b, b)
split = arr (\x -> (x,x))

unsplit :: (Arrow a) => (b -> c -> d) -> a (b, c) d
unsplit = arr . uncurry


-- (***) combines two arrows into a new arrow by running the two arrows on a pair of values (one arrow on the first item of the pair and one arrow on the second item of the pair).
f *** g = first f >>> second g

-- (&&&) combines two arrows into a new arrow by running the two arrows on the same value:
f &&& g = split >>> first f >>> second g

liftA2 :: (Arrow a) => (b -> c -> d) -> a e b -> a e c -> a e d
liftA2 op f g = split >>> first f >>> second g >>> unsplit op

f, g :: SimpleFunc Int Int
f = arr (`div` 2)
g = arr (\x -> x *3 + 1)

-- also by : liftA2 (+) f g
h :: SimpleFunc Int Int
h = liftA2 (+) f g

houtput :: Int
houtput = runF h 8
--    8 -> (8, 8)            split
--   (8, 8) -> (4, 8)        first f (x `div` 2 of the first element)
--   (4, 8) -> (4, 25)       second g (3*x + 1 of the second element)
--   (4, 25) -> 29           applies (+) to tuple elements.

-- SImplify h by do
h' :: SimpleFunc Int Int
h' =
    proc x -> do
        fx <- f -< x
        gx <- g -< x
        returnA -< (fx + gx)

hOutput :: Int
hOutput = runF h' 8



-- newtype Kleisli m a b = Kleisli {
--   runKleisli :: (a -> m b)
-- }
-- All Kleisli
-- comes complete with its own definitions for arr, first, second and (>>>). This means that all multi-value functions (a -> [b]) are already defined as Kleisli arrows (because [] is a monad)! (>>>) performs composition, keeping track of all the multiple results. Split, (&&&) and (***) are all defined as before.

plusminus , double , h2 :: Kleisli [] Int Int
plusminus = Kleisli (\x -> [x, -x])
double = arr (*2)
h2 = liftA2 (+) plusminus double

h2Out :: [Int]
h2Out = runKleisli h2 8
