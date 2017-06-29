

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
