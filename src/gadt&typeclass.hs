{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

{-# LANGUAGE RankNTypes                 #-}

{-# LANGUAGE DataKinds                  #-}

{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TypeFamilies               #-}



module GADTANDTYPECLASS where


import           Control.Applicative
import           Criterion.Main


-- Data Kinds
data Nat = Zero | Succ Nat

-- Return Type Polymorphism
class CanFail failable where
    oops :: failable a
    pick :: failable a -> failable a -> failable a
    win :: a -> failable a

instance CanFail Maybe where
    oops = Nothing
    pick (Just a) _       = Just a
    pick Nothing (Just a) = Just a
    pick Nothing Nothing  = oops
    win a = Just a

testFailure :: CanFail failable => Double -> Double -> failable Double
testFailure a b = if a == 0
    then oops
    else win (a+b)
-- testFailure 2 5 :: Maybe Double  or Either Double or [Double]

someMathFunction :: Double -> Double -> Double
someMathFunction x y =
    let result = testFailure x y in
    case result of
        Just number ->
            number * 3
        Nothing ->
            0










-- Using Rank Types
data X = Y Integer
       | Z Double
wrap :: (forall n. Num n => n -> n) -> X -> X
wrap f (Y i) = Y $ f i
wrap f (Z i) = Z $ f i



data Encrypted
data PlainText

data Message a where
    EncryptedMessage :: String -> Message Encrypted
    PlainMessage :: String -> Message PlainText

messageLength :: Message a -> Int
messageLength (EncryptedMessage a) = length a
messageLength (PlainMessage a)     = length a


sendMessage :: Message Encrypted -> IO ()
sendMessage (EncryptedMessage a) = putStrLn a




-- Generalised Data Type
data Exp a where
    I :: Int -> Exp Int
    B :: Bool -> Exp Bool
    Add :: Exp Int -> Exp Int -> Exp Int
    Mul :: Exp Int -> Exp Int -> Exp Int
    Equ :: Exp Int -> Exp Int -> Exp Bool

eval :: Exp a -> a
eval (I n)       = n
eval (B b)       = b
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2
eval (Equ e1 e2) = eval e1 ==  eval e2







--- Momization
memoize :: (Int -> a) -> (Int -> a)
memoize f = (map f [0 ..] !!)



sqaures :: (Num a, Num b, Num c) => (a,b,c) -> (a,b,c)
sqaures (x, y, z) = (x*x, y*y, z*z)

-- Our benchmark harness.
main = defaultMain [
  bgroup "sqaures" [ bench "1"  $ whnf sqaures (1000, 2000, 5000)
               , bench "5"  $ whnf sqaures (5000, 1000, 2000)
               ]
  ]


class Num a => Bum a where
    one :: a
    addd :: a -> a -> a

instance Bum Int where
    one = 24
    addd x y = x + y


-- more than one type var
-- {-# LANGUAGE MultiParamTypeClasses #-}
class Coerce a b where
    coerce :: a -> b

instance Coerce Int Float where
    coerce  =  fromIntegral


class Animal a b where
    eat :: a -> b -> Int



-- A partial function is a function that is not defined for all possible arguments of the specified type. Examples in the Haskell standard library are:
-- head, tail: undefined for empty lists




class TooMany a where
    tooMany :: a -> Bool



-- we need to explicity tell -  tooMany (10 :: Int)
-- now
newtype Goat = Goat Int deriving Show

instance TooMany Goat where
    tooMany (Goat n) = n > 2


-- tooMany (Goat 10)

-- not with type synonym

-- {-# LANGUAGE FlexibleInstances          #-}
-- Flexibleinstancesis used when a resided inside a monadic structure
-- eg -- [Char]
-- Recall that String is a synonym for [Char], which in turn is the type [a] where Char is substituted for the type parameter a. According to Haskell 98's rules, we are not allowed to supply a type in place of a type parameter when we write an instance. In other words, it would be legal for us to write an instance for [a], but not for [Char].
-- The problem is that Haskell 98 says you can't use a type synonym in an instance declaration.
type Cow = Int
instance TooMany Cow where
    tooMany n = n > 2

-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
newtype Pig = Pig Int
     deriving (Eq, Show, TooMany)


-- {-# LANGUAGE FlexibleInstances          #-}
instance TooMany (Int, String) where
    tooMany (x, y) = x > 2



tool = do
    let ab :: (Int, String)
        ab = (10, "Hello")
    print $ tooMany ab

-- Also we can do without FlexibleInstance by using newtype
newtype Tup = Tup (Int, String)

instance TooMany Tup where
    tooMany (Tup(x,y)) = x > 2















type Option = String
data Config = MakeConfig [Option] deriving (Show)
--------

configBuilder :: [Option] -> Config
configBuilder options = MakeConfig options

defaultConfig' :: [Option] -> Config
defaultConfig' options = MakeConfig (["-Wall"] ++ options)


profile  :: ([Option] -> Config) -> ([Option] -> Config)
profile builder =
    \options -> builder (["-prof", "-auto-all"] ++ options)

goFaster :: ([Option] -> Config) -> ([Option] -> Config)
goFaster builder =
    \options -> builder (["-O2"] ++ options)

extract :: ([Option] -> Config) -> Config
extract builder = builder []


-- (&) is another popular name for this operator
(#) :: a -> (a -> b) -> b
x # f = f x

infixl 0 #


builder1 = defaultConfig' # profile
builder2 = builder1 # goFaster







-------------------- TypeFamlies
-- Type families come in two flavors: data families and type synonym families.
-- Data Family : data and newtype definitions
-- Type Family - type synonym


{--
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts               #-}
--}

{--
Without FlexibleContexts all typeclass constraints on function definitions must have type variables. For example:

add :: Num a => a -> a
add = (+)
Where a is the type variable. With FlexibleContexts enabled you can have any type inside a typeclass.

intAdd :: Num Int => Int -> Int
intAdd = (+)
This example is pretty contrived but it is the simplest I can think of. FlexibleContexts is usually only used with MultiParamTypeClasses. Here is an example:

class Shower a b where
  myShow :: a -> b

doSomething :: Shower a String => a -> String
doSomething = myShow
Here you can see we say that we only want a Shower a String. Without FlexibleContexts String would have to be a type variable instead of a concrete type.
--}





data Fire = Charmander | Charmeleon | Charizard deriving Show
data Water = Squirtle | Wartortle | Blastoise deriving Show
data Grass = Bulbasaur | Ivysaur | Venusaur deriving Show

-- instead of writinf here write in instance of class
-- data FireMove = Ember | FlameThrower | FireBlast deriving Show
-- data WaterMove = Bubble | WaterGun deriving Show
-- data GrassMove = VineWhip deriving Show

{-- Instead of
class (Show pokemon, Show move) => Pokemon pokemon move where
  pickMove :: pokemon -> move
--}

-- we can write
-- here we have an associated type i.e MOve type
class (Show a, Show (Move a)) => Pokemon a where
  data  Move a :: *
  pickMove :: a -> Move a


instance Pokemon Fire where
    data Move Fire = Ember | FlameThrower | FireBlast deriving Show
    pickMove Charmander = Ember
    pickMove Charmeleon = FlameThrower
    pickMove Charizard  = FireBlast


instance Pokemon Water where
  data Move Water = Bubble | WaterGun deriving Show
  pickMove Squirtle = Bubble
  pickMove _        = WaterGun

main' :: IO ()
main' = do
  print $ pickMove Charmander


printBattle :: String -> String -> String -> String -> String -> IO ()
printBattle pokemonOne moveOne pokemonTwo moveTwo winner = do
    putStrLn $ pokemonOne ++ " used " ++ moveOne
    putStrLn $ pokemonTwo ++ " used " ++ moveTwo
    putStrLn $ "Winner is: " ++ winner ++ "\n"

{--
class (Pokemon pokemon, Pokemon foe) => Battle pokemon foe where
    battle :: pokemon -> foe -> IO ()
    battle pokemon foe = do
      printBattle (show pokemon) (show move) (show foe) (show foeMove) (show pokemon)
     where
      foeMove = pickMove foe
      move = pickMove pokemon
--}

-- Including Associated Type Synonyms
class (Show (Winner pokemon foe), Pokemon pokemon, Pokemon foe) => Battle pokemon foe where
  type Winner pokemon foe :: *  -- this is the associated type
  type Winner pokemon foe = pokemon -- -- this is the default implementation

  battle :: pokemon -> foe -> IO ()
  battle pokemon foe = do
    printBattle (show pokemon) (show move) (show foe) (show foeMove) (show winner)
   where
    foeMove = pickMove foe
    move = pickMove pokemon
    winner = pickWinner pokemon foe

  pickWinner :: pokemon -> foe -> (Winner pokemon foe)



instance Battle Water Fire where
    pickWinner pokemon foe = pokemon

instance Battle Fire Water where
    type Winner Fire Water = Water
    pickWinner = flip pickWinner
