{-# LANGUAGE TypeFamilies #-}

module FoldDiff where

import           Data.Foldable
import           Data.List     (foldl')


foo :: Int -> Int -> Int
foo _ 0 = 0
foo x y = x*y

okk = foldl foo 1 [1,2, undefined,0]
ohh = foldl' foo 1 [1,2, undefined,0] -- exception


-- more about folds and monoid

newtype Pendo a = Pendo { appPendo :: a -> a }


instance Monoid (Pendo a) where
    mempty = Pendo id
    Pendo f `mappend` Pendo g = Pendo (f . g)

f = fold [ Pendo (+ 3)
         , Pendo (`div` 2)
         , Pendo (+ 10)
         ]



-- from library for tuple
-- instance (Monoid a, Monoid b) => Monoid (a, b) where
--     mempty = (mempty, mempty)
--     (a1, b1) `mappend` (a2, b2) = (a1 <> a2, b1 <> b2)







--
data Boost' a = Boost a
    deriving (Eq, Show)
type Boost = Boost' Integer

instance Functor Boost' where
    fmap f (Boost a) = Boost (f a)




data Bar a b = Bar a b
        deriving (Eq, Show)

instance Functor (Bar a) where
   fmap f (Bar a b) = Bar a (f b)


data Moo a = Moo a a
       deriving (Eq, Show)

instance Functor Moo where
  fmap f (Moo a b) = Moo (f a) (f b)

data Book = Book {-# UNPACK #-}!Int {-# UNPACK #-}!Int


data Langauge = Litreature String | Maths Integer
        deriving (Eq, Show)
-- instance Functor Langauge where
--     fmap f (Litreature s) = Litreature (f s)
--     fmap f (Maths s)      = Maths (f s)



data family XList a


data Price  a = Price Integer deriving (Eq, Show)

-- instance Functor Price where
--     fmap f (Price a) = Price (f a)
