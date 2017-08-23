{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies      #-}

module DefaultSigns where

--

{-
-- | This is OK
class C a where
  f :: [a]
  default f :: (Num a) => [a]
  f = [1,2,3,4,5]

-- | But this is not
-- The only trouble you might encounter is that when your type is so
specific that it doesn't even mention the type variables of the class,
GHC will complain. This isn't allowed:

class C a where
  f :: [a]
  default f :: [Int]
  f = [1,2,3,4,5]

-}

-- (a ~ Int) means imporve has some type 'a' where 'a' is an Int

class Improve a where
    improve :: a   -- TypeFamilies
    default improve :: (a ~ Int ) =>  a  -- DefaultSignature
    improve   = 2

instance Improve Int
