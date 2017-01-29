{-# LANGUAGE ExistentialQuantification #-}

module BeautifulFold where

import qualified Data.Bits
import qualified Data.Foldable
import           Data.Monoid
import           Data.Word     (Word64)
import           Prelude       hiding (product, sum)

data Fold i o = forall m . Monoid m => Fold (i -> m) (m -> o)


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
main = print (fold (unique id) (take 100000 (cycle randomWord64)))

randomWord64 :: [Word64]
randomWord64 = [2456537040202043740, 1251112620038251420, 1023776523434540, 2938000003949, 1209304889200101]



-- Combining Folds
combine :: Fold i a -> Fold i b -> Fold i (a, b)
combine (Fold tallyL summariseL) (Fold tallyR summariseR) = Fold tally summarise
    where
        tally x = (tallyL x, tallyR x)
        summarise (sL, sR) = (summariseL sL, summariseR sR)
--- eg fold (combine sum product) [1..10]
