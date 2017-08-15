{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TupleSections          #-}



module Lens1 where

import           Control.Applicative
import           Data.Functor.Contravariant
import           Data.Foldable
import           Control.Monad.Identity
import           Data.Bifunctor
import           Data.Functor
import           Data.Functor.Compose
import           Data.Functor.Const
import           Data.Monoid
import           Data.Time.Clock
import           System.Environment
import           Text.Printf



-- | Lens 1
data Lens1 s a = Lens1
    { getter :: s -> a
    , setter :: a -> s -> s }

setIth :: Int -> a -> [a] -> [a]
setIth index new list
    | index < 0 = error "negative Index"
    | null list = error "Index too large"
    | old : rest <- list = if index == 0
                                then new : rest
                                else old : setIth (index - 1) new rest

ix1 :: Int -> Lens1 [a] a
ix1 index = Lens1 { getter = (!! index)
                 , setter = setIth index }


-- | Lens 2
type Lens2 s a = (a -> a) -> s -> (a, s)

ix2 :: Int -> Lens2 [a] a
ix2 index f list
    | index < 0 = error "negative Index"
    | null list = error "Index too large"
    | old : rest <- list = if index == 0
                                then (,) old $ f old : rest
                                else second (old :) $ ix2 (index - 1) f rest
                                -- fmap will work same as second for (,)


-- | Lens3                      using Functor
-- Rank type to constraint to type synonyms
-- all monads are functor not vice versa

type Lens3 s a = forall  f. Functor f => (a -> f a) -> s -> (a, f s)

ix3 :: Int -> Lens3 [a] a
ix3 index f list
    | index < 0 = error "negative Index"
    | null list = error "Index too large"
    | old : rest <- list = if index == 0
                                then (,) old $ (: rest) <$> f old
                                else second ((old :) <$>) $ ix3 (index - 1) f rest


-- | Lens4                      Setter is Getter

type Lens4 s a = forall f. Functor f => (a -> f a) -> s -> f s

ix4 :: Int -> Lens4 [a] a
ix4 index f list
    | index < 0 = error "negative Index"
    | null list = error "Index too large"
    | old : rest <- list = if index == 0
                                then (: rest) <$> f old
                                else (old :) <$> ix4 (index - 1) f rest

-- ix4 1 (\x -> (,) x x) [1,2,3,4]


-- | Lens                       s t a b
type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s


-- | Composing Functor    Compose f g a
--  ix4 1 (\x -> Compose ((,) x (Just x)) ) [1,2,3,4]


-- | Identity Functor
-- Setting    ix4 1 (\x -> Identity 99 ) [1,2,3,4]   -- a
-- Modifying  ix4 1 (\x -> Identity (x * 10) ) [1,2,3,4]
-- | Over
over :: Lens s t a b -> ((a -> b) -> s -> t)
over l f = runIdentity . l (Identity . f)

-- over (ix4 2) (+100) [1,2,3]
-- over (ix4 2) (const 10) [1,2,3]                  -- a



-- | Test Yourself

_1 :: Lens (a,x) (b,x) a b
_1 f (t, t1) = (, t1) <$> f t

_2 :: Lens (x,a) (x,b) a b
_2 f (t, t1) = (t,) <$> f t1

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens get set f s = set s <$> f (get s)


choosing :: Lens s1 t1 a b -> Lens s2 t2 a b
         -> Lens (Either s1 s2) (Either t1 t2) a b
choosing l1 l2 f s = case s of
    Left s1  -> Left <$> l1 f s1
    Right s2 -> Right <$> l2 f s2

-- modify and return  (+9 )(1,2) => (10, (10, 2))
(<%~) :: Lens s t a b -> (a -> b) -> s -> (b, t)
(<%~) l f s = l  (liftA2 (,) f f) s


(<<%~) :: Lens s t a b -> (a -> b) -> s -> (a, t)
(<<%~) l f s = l (liftA2 (,) id f) s

united :: Lens' s ()
united f s = const s <$> f ()



-- | Traversal -- focus on several elements of list a s long as they are equal.

_all :: Eq a => a -> Lens' [a] a
_all ref = lens get set
    where
        get s = ref
        set s new = map (\x -> if x == ref then new else x) s
-- over (_all 1) (const 100) [1,8,1,3]


-- | Applicative  -- the power of Const with Monoid
type AppLens s t a b = forall f. Applicative f => (a -> f b) -> s -> f t

type AppLens' s a = AppLens s s a a

_all' :: Eq a => a -> AppLens' [a] a
_all' ref f s = traverse update s
    where
        update old = if old == ref then f old else pure old

-- (_all' 0) (const $ putStr "? new: " >> readLn) [100, 600, 0, 200, 0]




-- | view set get
type Getting s a = ((a -> Const a a) -> s -> Const a s)
type Setting s t a b = ((a -> Identity b) -> s -> Identity t)

view :: Getting s a -> s -> a
view l = getConst . l Const

over' :: Setting s t a b -> (a -> b) -> s -> t
over' l f = runIdentity . l (Identity . f)

set :: Setting s t a b -> b -> s -> t
set l  x = runIdentity . l (Identity . const x )


-- As view and all will accumulate the value hence we need a Monoid
toListOf :: ((a -> Const [a] a) -> s -> Const [a] s) -> s -> [a]
toListOf l = getConst . l (Const  . (: []))
-- toListOf (_all' 1) [1,2,3,1,1]
{--
getConst $ traverse (\x -> if x == 0 then (Const [x]) else pure x) [1,0,3,0,4,0]
                                           here [] is Monoid
                            for traverse (Applicative f)
                            Monoid m => Applicative (Const m)
--}



-- | Preview -- First Value
preview :: ((a -> Const (First a) a) -> s -> Const (First a) s) -> s -> Maybe a
preview l = getFirst . getConst . l (Const . First . Just)
-- preview (_all' 0) [1,2,2]   => Nothing


-- | HAS -- Check for value
has :: ((a -> Const Any a) -> s -> Const Any s) -> s -> Bool
has l = getAny . getConst . l (\_ -> Const (Any True))

-- has (_all' 0) [1,2,9]

hasSum :: Getting' (Sum a) s a -> s -> a
hasSum l = getSum . getConst . l (Const . Sum)
-- hasSum (_all' 1) [1,2,1,3,1]

type Getting' r s a = (a -> Const r a) -> s -> Const r s
{-- Now a better way would be

view :: Getting a s a -> s -> a
toListOf :: Getting [a] s a -> s -> [a]
preview  :: Getting (First a) s a -> s -> Maybe a
has      :: Getting Any s a -> s -> Bool

--}

-- Endo is `(.)` and `id`
-- Anything with "endo" means "back to the same thing", while "morphism" means "shape changer"
toListOf' :: Getting' (Endo [a]) s a -> s -> [a]
toListOf' l = (`appEndo` []) . getConst . l (\x -> Const (Endo (x:)))








-- | AppLens IS TRAVERSAL LENS

type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t

type Traversal' s a = Traversal s s a a


-- each = every Element
class Each s t a b | s -> a, t -> b, s b -> t, t a -> s where
    each :: Traversal s t a b



--                             functionalInstances  i.e [Char], Maybe Int
instance Traversable t => Each (t a) (t b) a b where
    each = traverse  --but as travrse on work on second element of tuple hence







-- | Head Last
_head :: Traversal' [a] a
_head f [] = pure []
_head f (x:xs) = (:) <$> f x <*> pure xs







-------------------------------------------
-------------------------------------------
----------- LENS COMPOSITION --------------
-------------------------------------------
-------------------------------------------

(@.) :: Lens' b c -> Lens' a b -> Lens' a c
(@.) = flip (.)




-- | Lens Law
-- 1.    view l ( set l v s) = v        You get back what you put in:
--  toListOf' (_all' 100) (set (_all' 1) 100 [2,3, 1])   => [100]

-- 2. set l (view l s) s ≡ s        Putting back what you got doesn't change anything:
-- 3. set l v' (set l v s) ≡ set l v' s     Setting twice is the same as setting once:


-- let fmap2 f = getCompose . fmap f . Compose  ~ as fmap . fmap





-------------------------------------------
--------------- FOLD ----------------------

type Fold s a = forall f. (Contravariant f, Applicative f) => (a -> f a) -> s -> f s



-------------------------------------------
---------- isomorphisms, some profunctors, lens families

-- isomorphic    (s -> a) => (a -> s)

newtype Tagged a b = Tagged {unTagged :: b}

enum :: (Enum a, Functor f) => Tagged a (f a) -> Tagged Int (f Int)
enum (Tagged fa) = Tagged (fromEnum <$> fa)
