{-# LANGUAGE KindSignatures #-}
-- This is needed so that we can have constraints in type synonyms.
{-# LANGUAGE RankNTypes     #-}

module LensPractice where

import           Control.Applicative
import           Control.Monad.Identity
import           Data.Bifunctor
import           Data.Functor.Compose
import           Data.Functor.Const
import Data.Functor


-- lens over tea 1
-- Getter + Setter = Lens
------------------------------
data Lens s a = Lens
    { getter :: s -> a
    , setter :: a -> s -> s }

setIth :: Int -> a -> [a] -> [a]
setIth index new list
    | index < 0 = error "Negative Index"
    | null list = error "Index too large"
    | old : rest <- list = if index == 0
                                then new : rest
                                else old : setIth (index - 1) new rest

ix :: Int -> Lens [a] a
ix i = Lens { getter = (!! i)
            , setter = setIth i }

-- Removing the Getter
------------------------------
type Lens' s a = (a -> a) -> s -> (a, s)

-- ixx :: Int -> (a -> a) -> s -> (a, s)
ixx :: Int -> Lens' [a] a
ixx index f list
    | index < 0 = error "Negative Index"
    | null list = error "Index too large"
    | old : rest <- list = if index == 0
                                then (old, f old : rest)
                                else second (old :) $ ixx (index - 1) f rest
-- getter    ixx 0 (id) [2,3]
-- setter    ixx 0 (const 10) [2,3]
-- modifier  ixx 0 (+ 10) [2,3]


-- Functor to Rescue
---------------------------
type Lens'' s a = forall f. Functor f => (a -> f a) -> s -> (a, f s)

ixxx :: Int -> Lens'' [a] a
ixxx index f list
    | index < 0 = error "Negative Index"
    | null list = error "Index too large"
    | old : rest <- list = if index == 0
                                then (old, (: rest) <$> f old )
                                else second ((old :) <$>) $ ixxx (index - 1) f rest

-- Setter is Getter
-----------------------
data Storey x f a = Storey x (f a) -- same as (x, fa)
    deriving Show

instance Functor f => Functor (Storey x f) where
    fmap g (Storey x fa) = Storey x (fmap g fa)

type Lens''' s a = forall f. Functor f => (a -> f a) -> s -> f s

ixxxx :: Int -> Lens''' [a] a
ixxxx index f list
    | index < 0 = error "Negative Index"
    | null list = error "Index too large"
    | old : rest <- list = if index == 0
                                then (: rest) <$> f old
                                else (old :) <$> ixxxx (index - 1) f rest
-- ixxxx 0 (\x -> Storey x [1..x]) [4,5,8,3]





-----------------------------------------------------------------
type LensR s t a b = forall f. Functor f => (a -> f b) -> s -> f t
-- type Lens s a = Functor f => (a -> f a) -> s -> f s  -- Simple
-----------------------------------------------------------------


-- Composing Functors  (.)
-----------------------------------------
-- data Storey x f = Storey (,) x . f
-- data Compose f g a = Compose f (g a)
-- getCompose $ ixxxx 2 (\x -> Compose (x, [1..x])) [300, 100, 4, 600, 900, 400]


-- What if I don't need any functors?
------------------------------------------
-- Use Identity
-- runIdentity $ ixxxx 2 (\x -> Identity 850) [300, 100, 4, 600, 900, 400]

-------------------------------------------
over :: LensR s t a b -> ((a -> b) -> s -> t)
over l f = runIdentity . l (Identity . f)

-- runIdentity $ ixxxx 2 (\x -> Identity (const 850 x) ) [300, 100, 4, 600, 900, 400]
foo :: IO ()
foo = do
    let set = over (ixxxx 2) (const 88) [0..4]
    let modify = over (ixxxx 2) (* 44) [0..4]
    print (set, modify)


-- What if I don't need any modifications?
---------------------------------------------
-- Const :: a -> Const a b
-- getConst :: Const a b -> a
{--
data Storey x f a = Storey x (f a)
data Const x a = Const x
--}

--  humanitarian aid, with a letter glued to the box
getByStorey lens s = x where
    Storey x _ = lens (\x -> Storey x (Identity x)) s

{--
Also called view in lens
letter, except that... there's no humanitarian aid,
--}
getByConst lens s = x where
    Const x = lens (\x -> Const x) s




-----------------------------------------------------------
-- type LensR s t a b = forall f. Functor f => (a -> f b) -> s -> f t
-- Questions
type Lens1 s a = LensR s s a a

-- bar :: Functor f => (a -> fb) -> (a, x) -> (b, x)
-- bar1 :: LensR (a, x) (b, x) a b
-- bar1 f (t, t2) =


-- bar2 :: Functor f => (a -> fb) -> (x, a) -> (x, b)
bar2 :: LensR (x, a) (x, b) a b
bar2 f (t, t2) =  (,) t <$> f t2
-- runIdentity $ bar2 (\x -> Identity (x +1) ) (1,2


-- lensSG :: (s -> a) -> (s -> b -> t) -> LensR s t a b
-- lensSG get set =

{--
type LensR s t a b = forall f. Functor f => (a -> f b) -> s -> f t
over :: LensR s t a b -> ((a -> b) -> s -> t)
over l f = runIdentity . l (Identity . f)

runIdentity $ ixxxx 2 (\x -> Identity (const 850 x) ) [300, 100, 4, 600, 900, 400]
--}
(<%~) :: LensR s t a b -> (a -> b) -> s -> (b, t)
(<%~) l f s = l ((,) <$> f <*> f)  s

(<<%~) :: LensR s t a b -> (a -> b) -> s -> (a, t)
(<<%~) l f s = l ((,) <$> id <*> f) s -- also  ((,) <*> f) or (\x -> (x, f x))




--------------------------------------------------------------------
--                        Traversal 101
-- focus on sever element of List as long as they are equal
-- type Lens1 s a = LensR s s a a

type AppLens s t a b = forall f. Applicative f => (a -> f b) -> s -> f t
type AppLens' s a = AppLens s s a a

-- for MOnadic
_all :: (Monad m, Eq a) => a -> (a -> m a) -> [a] -> m [a]
_all ref f s = mapM update s
    where
        update old = if old == ref then f old else pure old
-- _all 0 (\x -> Identity (x+1)) [1,0,9,8,0]


-- for Applicative
_all' :: Eq a => a -> AppLens' [a] a
_all' ref f s = traverse update s
    where
        update old = if old == ref then f old else pure old


choice = _all' 0 (const $ putStr "? new: " >> readLn) [1,0,9,8,0]





-------------------------------------------------------------------
--              View, Over and Set
{--
view :: Lens s t a b -> s -> a        -- uses Const
    --  ((a -> f a) -> s -> f s) -> s -> s
        ((a -> Const a a) -> s -> Const a s) -> s -> a

over :: Lens s t a b -> (a -> b) -> s -> t  -- use Identity
    --  ((a -> f b) -> s -> f t) -> (a -> b) -> s -> t
    --  ((a -> Identity b) -> s -> Identity t) -> (a -> b) -> s -> t

set :: Lens s t a b -> b-> s-> t
--}

type Getting s a = (a -> Const a a) -> s -> Const a s
type Setting s t a b = (a -> Identity b) -> s -> Identity t

{--
view :: Getting s a -> s -> a
over :: Setting s t a b -> (a -> b) -> s -> t
set :: Setting s t a b -> b -> s -> t
--}
