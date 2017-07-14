
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module TypeClasses where



-- http://stackoverflow.com/questions/4469615/inclusion-of-typeclasses-with-default-implementation-in-haskell

class Foo a where
    foo :: a -> Int

-- 'a' belongs to 'Bar' only if it belongs to 'Foo' also
class Foo a => Bar a where
    bar :: a -> [Int]
    bar x = [foo x] -- yes, you can specify default implementation

instance Foo Char where
    foo _ = 0

-- instance with default 'bar' implementation
instance Bar Char

{-- Since empty not mentions a hence below will not work
class Coll s a where
    empty  :: s
     --  empty :: Coll e a => s -- By "ambiguous" we mean that there is a type variable a that appears on the left of the => symbol, but not on the right. The problem with this is that, according to the theoretical foundations of Haskell overloading, we cannot guarantee a well-defined semantics for any term with an ambiguous type.

    insert :: s -> a -> s
--}
-- Hence either use functional dependency or write as below
-- Here s -> a means "s determines a"
class Loll s a | s -> a where
    emptyl  :: s
    insertl :: s -> a -> s
-- Above :: We're saying "Look, if you determine what s is, then there is a unique a so that Loll s a exists so don't bother trying to infer a, just go look up the instance and typecheck that". This let's the type inferencer by much more effective and helps inference in a number of places.

-- OR
class CollE s where
  empty  :: s

class CollE s => Coll s a where
  insert :: s -> a -> s
