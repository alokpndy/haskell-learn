{-# LANGUAGE KindSignatures #-}
-- This is needed so that we can have constraints in type synonyms.
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE
  MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies #-}
{-# LANGUAGE DefaultSignatures #-}

module LensPractice1 where

import           Control.Applicative
import           Control.Monad.Identity
import           Data.Bifunctor
import           Data.Functor
import           Data.Monoid
import           Data.Functor.Compose
import           Data.Functor.Const

getFirst' = fst (1, True)
setFirst' x = (x, snd(1, True))

-------------------------- GETTER SETTER

data Lens1 s a = Lens1
    { getter :: s -> a
    , setter :: a -> s -> s }

setIth :: Int -> a -> [a] -> [a]
setIth index new list
    | index < 0 = error "Negative Index"
    | null list = error "Index too large"
    | old : rest <- list = if index == 0
                                then new : rest
                                else old : setIth (index - 1) new rest

lensIth :: Int -> Lens1 [a] a
lensIth index = Lens1
    { getter = (!! index)
    , setter = setIth index }

--------------------------- REMOVING THE GETTER
type Lens2 s a = (a -> a) -> s -> (a, s)

ix :: Int -> Lens2 [a] a
ix index f list
    | index < 0 = error "Negative Index"
    | null list = error "Index too large"
    | old : rest <- list = if index == 0
                                then (old, f old : rest)
                                else second (old :) $ ix (index - 1) f rest
                                -- or fmap (old :) $ ix (index - 1) f rest
                                -- or (old :) <$> (ix (index - 1) f rest)

--------------------------- FUNCTOR TO RESCUE ON NESTED LIST
type Lens3 s a = forall f. Functor f => (a -> f a) -> s -> (a, f s)

ix2 :: Int -> Lens3 [a] a
ix2 index f list
    | index < 0 = error "Negative Index"
    | null list = error "Index too large"
    | old : rest <- list = if index == 0
                                then (old, (: rest) <$> f old)
                                else second ((old :) <$>) (ix2 (index - 1) f rest)
                                --   second  ((4 :) <$>) (a, [[9]])
                                --                            f s or f []

----------------------------- SETTER IS GETTER
data Storey x f a = Storey x (f a )
    deriving Show
instance Functor f => Functor (Storey x f) where
    fmap f (Storey x fa) = Storey x (fmap f fa)

-- Now Our Lens becomes
type Lens4 s a = forall f. Functor f => (a -> f a) -> s -> f s

ix3 :: Int -> Lens4 [a] a
ix3 index f list
    | index < 0 = error "Negative Index"
    | null list = error "Index too large"
    | old : rest <- list = if index == 0
                                then (: rest) <$> f old
                                else (old :) <$> ix3 (index - 1) f rest


---------------------------- WHY PRESERVE THE TYPE ON WHICH FOCUSING
-- Change Structure from s to t
type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
type Lens5 s a = forall f. Functor f => (a -> f a) -> s -> f s

--------------------------- COMPOSING FUNCTORS
-- Storey x (f a ) ~ Comppose ((,) x) f   or Compose ((,) x . f)
-- Since ((,) x . f), we have to write fmap.fmap
-- Instead we wrap it in Compose and thwn we will get functor  instance free

nowThis = getCompose $ ix3 1 (\x -> Compose (x, [1..x])) [2,4,5,6]


---------------------------- WHAT IF DONT WANT FUNCTOR
-- Use Identity
setting = runIdentity $ ix3 1 (\x -> Identity 100) [2,4,5,6] -- over (ix3 1) (const 100) [1..3]
modifing = runIdentity $ ix3 1 (\x -> Identity (x + 50)) [2,4,5,6]

---------------------------- OVER
over :: Lens s t a b -> ((a -> b) -> s -> t)
over l f = runIdentity . l (Identity . f)
--                           (a -> Identity a)


--------------------------- WHAT ID DONT NEED MODIFICATION -- USE CONST
getByStorey lens s = x
    where
        Storey x _ = lens (\x -> Storey x (Identity x)) s

-- VIEW IN LENS
getByConst lens s = x
    where
        Const x = lens (\x -> Const x) s



--------------------------- PRACTICE
--  type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
type Lens6 s a = Lens s s a a -- s t a b

-- _1 :: forall f. Functor f => (a -> f b) -> (a, x) -> f (b, x)
_1 :: Lens (a, x) (b, x) a b
_1 f (a, x) = (\y -> (y, x)) <$> f a  -- or we can use TupleSections extension


-- _2 :: forall f. Functor f => (a -> f b) -> (x, a) -> f (x, b)
_2 :: Lens (x, a) (x, b) a b
_2 f (x, a) = (,) x <$> f a

makeLens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
makeLens get set f s = set s <$> f (get s)


eitherOr :: Lens s1 t1 a b -> Lens s2 t2 a b -> Lens (Either s1 s2) (Either t1 t2) a b
eitherOr l1 l2 f s = case s of
    Left s1  -> Left <$> l1 f s1
    Right s2 -> Right <$> l2 f s2

-- modify the target of lens and return the result
(<%~) :: Lens s t a b -> (a -> b) -> s -> (b, t)
(<%~) l f s = l (liftA2 (,) f f) s  -- or l ((,) <$> f <*> f) s or l (f &&& g)

-- modify the target of lens but return old value
(<<%~) :: Lens s t a b -> (a -> b) -> s -> (a, t)
(<<%~) l f s = l (\x -> (x, f x)) s  ---or l ((,) <*> f ) s -- or l (liftA2 (,) id f) s

-- (() -> f ()) -> s -> f ()
united :: Lens6 s ()
united  f s = const s <$> f ()




------------------------------ TRAVERSAL 101
-- focus on several elements of list a s long as they are equal

_all :: Eq a => a -> Lens6 [a] a
_all ref = makeLens get set
    where
        get s = ref
        set s new = map (\old -> if old == ref then new else old) s
-- over (_all 0) (const 100)  [1,0,3,0,5]



----------------------------- APPLICATIVE
type AppLens s t a b = forall f. Applicative f => (a -> f b) -> s -> f t
type AppLens' s a = AppLens s s a a

----------------------------- WRITING A BETTER _ALL
--  _all' :: (Applicative f, Eq a) => a -> (a -> f a) -> [a] -> f [a]
_all' :: Eq a => a -> AppLens' [a] a
_all' ref f list = traverse update list
    where
        update old = if old == ref then f old else pure old

{--
getConst $ traverse (\x -> if x == 0 then (Const [x]) else pure x) [1,0,3,0,4,0]
                                           here [] is Monoid
                            for traverse (Applicative f)
                            Monoid m => Applicative (Const m)
                            hence m must be monoid
                            which also helps collect values
--}

-- (>>) is applicative
nowUsingAppl = (_all' 0) (const $ putStr "? new: " >> readLn) [100, 600, 0, 200, 0]




---------------------------- VIEW OVER AND SET

type Getting s a = (a -> Const a a) -> s -> Const a s
type Setting s t a b = (a -> Identity b) -> s -> Identity t

-- (_all' 0) (const $ putStr "? new: " >> readLn) [100, 600, 0, 200, 0]
-- It will not works with over as it require Functor not Applicatove which AppLens require
-- Hence
view' :: ((a -> Const a a) -> s -> Const a s) -> s -> a
view' l  = getConst . l Const

view'' :: Getting s a -> s -> a
view'' l  = getConst . l Const

-- Original _all with Functor constraint
--  view' (_all 1) [1,0,3,0,5]    OK
-- BUt with Applicative constraint
--  view' (_all' 1) [1,0,3,0,5]  NOT OK

-- Monoid m => Applicative (Const m)
-- For Applicative of Const we get Monoid constraint, (not for Functor)
changeTo = view' (_all' [1]) [[1],[0],[3],[0],[5]]
--                     monoid


-------- To get all value toListOf
-- we will wrap a in monoid
toListOf :: ((a -> Const [a] a) -> s -> Const [a] s) -> s -> [a]
toListOf l  = getConst . l (\x -> Const [x])





over' :: ((a -> Identity b) -> s -> Identity t) -> (a -> b) -> s -> t
over' l f = runIdentity . l (Identity . f)

-- set is same as over'

-- Now we can
nowUsingAppl' = over' (_all' 0) (const 100)  [1,0,3,0,5]

-- thus
over'' :: Setting s t a b -> (a -> b) -> s -> t
over'' l f = runIdentity . l (Identity . f)




------------------------- PREVIEW == first value
{--
data First a = First { getFirst :: Maybe a }
            deriving (Eq, Show)

instance Monoid (First a) where
    mempty = First Nothing
    mappend (First Nothing) y = y
    mappend x _ = x

--}
-- First is already defined in Data.Monoid

preview :: ((a -> Const (First a) a) -> s -> Const (First a) s) -> s -> Maybe a
preview l = getFirst . getConst . l (\x -> Const (First (Just x)))

-- preview (_all' 0) []




-------------------------- HAS = CHECK FOR VALUE
-- USE ANY MONOID
{--
has :: ((a -> Const Any a) -> s -> Const Any s) -> s -> Bool
has l = getAny . getConst . l (\_ -> Const (Any True))
--}




------------------------- NO CLUMSY TYPE
{-- WE have already written
type Getting s a = (a -> Const a a) -> s -> Const a s
--}
-- Lets add Monoid intenstion to it
type Getting' r s a = (a -> Const r a) -> s -> Const r s

-- Now
view1 :: Getting' a s a -> s -> a
view1 l  = getConst . l Const

toListOf2 :: Getting' [a] s a -> s -> [a]
toListOf2 l =  getConst . l (\x -> Const [x])

preview2 :: Getting' (First a) s a -> s -> Maybe a
preview2 l = getFirst . getConst . l (\x -> Const (First (Just x)))





---------------------------- DIRRERENCE LIST
type ConcatList a = [[a]]
(+|+) :: ConcatList a -> ConcatList a -> ConcatList a
(+|+) a b = a ++ b

toList :: ConcatList a -> [a]
toList = concat



toListOf3 :: Getting' (Endo [a]) s a -> s -> [a]
toListOf3 l = (`appEndo` []) . getConst . l (\x -> Const (Endo (x:)))





---------------------------- GOING BACK TO TRAVERSAL
{--
type AppLens s t a b = forall f. Applicative f => (a -> f b) -> s -> f t
type AppLens' s a = AppLens s s a a
--}
-- Reqriting them
type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t
type Travesal' s a = Traversal  s s a a
type LensLike f s t a b = (a -> f b) -> s -> f t

---------------------------- each == every element
-- each focuses on every element in a monomorphic container:
--                      functional dependecies
class Each s t a b | s -> a, t -> b, s b -> t, t a -> s where
    each :: Traversal s t a b
    {--

#ifndef HLINT     -- if not defined       ~ is type equality
    default each :: (Applicative f, Traversable g, s ~ g a, t ~ g b)
                   => LensLike f s t a b
    -- or same as
    default each :: (Traversable g, s ~ g a, t ~ g b) => Traversal s t a b

    each = traversal
     {-# INLINE each #-}
#endif
    --}


{--
instance Each (a, a) (b, b) a b where
    each f ~(a,b) = (,) <$> f a <*> f b   -- here ~ means lazy pattern. See irrefutable pattern for more
--}


instance Traversable t => Each (t a) (t b) a b where
    each = traverse

{--
traverse  (\x -> Identity 2) [1,2,3]
each  (\x -> Identity 2) [1,2,3]
--}

-- As traverse == sequenceA . fmap f -- hence fmap only work on snd of (,)
-- We ahve a problem for tuple with each Lens






------------------------------ HEAD LAST
_head :: Travesal' [a] a
_head f [] = pure []
_head f (x:xs) = (:) <$> f x <*> pure xs
