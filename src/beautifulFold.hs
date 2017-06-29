{-# LANGUAGE ExistentialQuantification #-}


module BeautifulFold where

import           Control.Applicative
import qualified Data.Bits
import qualified Data.Foldable
import           Data.Monoid
import           Data.Word           (Word64)
import           Prelude             hiding (product, sum)


data Fold i o = forall m. Monoid m => Fold (i -> m) (m -> o)


instance Functor (Fold i) where
    fmap k (Fold tally summarise) = Fold tally (k. summarise)

instance Applicative (Fold i) where
    pure o = Fold (\_ -> ()) (\_ -> o)

    Fold tallyF summarizeF <*> Fold tallyX summarizeX = Fold tally summarize
      where
        tally i = (tallyF i, tallyX i)
        summarize (mF, mX) = summarizeF mF (summarizeX mX)




fold :: Fold i o -> [i] -> o
fold (Fold tally summarise) ls = summarise (reduce (map tally ls))
    where
        reduce = Data.Foldable.foldl' (<>) mempty

sum :: Num a => Fold a a
sum = Fold Sum getSum

product :: Num a => Fold a a
product = Fold Product getProduct

-- more straighforward -- not strict
sum' xs = getSum $ foldMap Sum xs


-- Safer Head :  foldr :: (a -> b -> b) -> b -> [a] -> b
-- or  also  fold (Fold  (First . Just) getFirst) []  ~ Nothing
--           fold (Fold  (Last . Just) getLast ) []



-- Unique elements using hyperlog log elements
newtype Max a = Max { getMax :: a}

instance (Bounded a, Ord a) => Monoid (Max a) where
    mempty = Max minBound
    mappend (Max x) (Max y) = Max (max y x)

unique :: (i -> Word64) -> Fold i Int
unique hash = Fold tally summarise
    where
        tally x = Max (fromIntegral (Data.Bits.countLeadingZeros (hash x)) :: Word64)
        summarise (Max x) = fromIntegral (2 * x)


main :: IO ()
main = print (fold (unique id) (take 10000 (cycle randomWord64)))

randomWord64 :: [Word64]
randomWord64 = [2456537040202043740, 1251112620038251420, 1080723776523434540, 2370564938000003949, 132209304889200101, 132209304089203100, 125119304889200101, 1080723776500404342]



-- Combining Folds
combine :: Fold i a -> Fold i b -> Fold i (a, b)
combine (Fold tallyL summariseL) (Fold tallyR summariseR) = Fold tally summarise
    where
        tally x = (tallyL x, tallyR x)
        summarise (sL, sR) = (summariseL sL, summariseR sR)
--- eg fold (combine sum product) [1..10]
-- or fold (combine (Fold  Sum getSum) (Fold  Product getProduct)) [1..10]


-- using instance
combine' :: Fold i a -> Fold i b -> Fold i (a, b)
combine' = liftA2 (,)

-- so now - fold (combine' (Fold Sum getSum) (Fold Product getProduct)) [1,2,3,4]
-- or fold ((,) <$> (Fold Sum getSum) <*> (Fold Product getProduct) ) [1..10]
-- also this will not materialise all steps at a time rather one at a step


-- also       fold ((,) <$> sum <*> product ) [1..10]
-- Above is better than (Prelude.sum xs, Prelude.product xs) because both sum and pro will not happen simultaneously and hence it will create bigger heap.









---

instance Num b => Num (Fold a b) where
    fromInteger a = pure (fromInteger a)

    negate = fmap negate
    abs = fmap abs
    signum = fmap signum

    (+) = liftA2 (+)
    (*) = liftA2 (*)
    (-) = liftA2 (-)


instance Fractional b => Fractional (Fold a b) where
    fromRational a = pure (fromRational a)

    recip = fmap recip

    (/) = liftA2 (/)


-- Now we can do
-- 1. fold (sum / product) [1..10]






gain = do
    putStrLn name
    where
         name = "Alok"
