module CovarianceAndContra where

import           Data.Functor


showInt :: MakeString Int
showInt = MakeString show

newtype MakeString a = MakeString { makeString :: a -> String }

mapMakeString :: (b -> a) -> MakeString a -> MakeString b
mapMakeString f (MakeString g) = MakeString (g . f)


plus3ShowInt :: MakeString Int
plus3ShowInt = mapMakeString (+3) showInt
