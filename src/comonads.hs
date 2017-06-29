module Comonads where

import           Control.Applicative



-- Builder Pattern
type Option = String
data Config = MakeConfig [Option] deriving Show


configBuilder :: [Option] -> Config
configBuilder = MakeConfig

--
defaultConfig :: [Option] -> Config
defaultConfig options = MakeConfig ( "-Wall" : options)


-- MAKE A function that can be modified later
-- this becomes option appenders and leave function open to take new options
profile :: ([Option] -> Config) -> ([Option] -> Config)
profile builder = \options -> builder ("-prof" : "-auto-all" : options)

goFaster :: ([Option] -> Config) -> ([Option] -> Config)
goFaster builder = \options -> builder ("-O2" : options)

-- Extracter or Getter
extract :: ([Option] -> Config) -> Config
extract builder = builder []  -- pass an empty option list

-- Creating method on an object
(#) :: a -> (a -> b) -> b
x # f = f x
infixl 0 #

-- Oops in Functional Programming
builder1 :: [Option] -> Config
builder1 = defaultConfig # profile

builder2 :: [Option] -> Config
builder2 = builder1 # goFaster

getter :: Config
getter = builder2 # extract




extend ::(([a] -> t1) -> t) -> ([a] -> t1) -> [a] -> t
extend setter builder =
    \opts2 -> setter (\opts1 -> builder (opts1 ++ opts2))







--- DSL
newtype Kelvin = Kelvin {getKelvin :: Double }

-- Let thermostat type be  :: (Kelvin, Kelvin -> a)

newtype Celsius = Celsius { getCelsius :: Double }
    deriving Show

kelvinToCelsius :: Kelvin -> Celsius
kelvinToCelsius (Kelvin t) = Celsius (t - 273.15)

initialThermostat :: (Kelvin, Kelvin -> Celsius)
initialThermostat = (Kelvin 298.15, kelvinToCelsius)

extractT :: (Kelvin, Kelvin -> a) -> a
extractT (t, f) = f t


-- Preview changes before making
up :: (Kelvin, Kelvin -> a) -> a
up (Kelvin t, f) = f $ Kelvin (t + 1)


down :: (Kelvin, Kelvin -> a) -> a
down (Kelvin t, f) = f $ Kelvin (t - 1)

toString :: (Kelvin, Kelvin -> Celsius) -> String
toString (t, f) = show (getCelsius (f t)) ++ " Celsius"


up' :: (Kelvin, Kelvin -> a) -> (Kelvin, Kelvin -> a)
up' (Kelvin t, f) = (Kelvin (t + 1), f)

down' :: (Kelvin, Kelvin -> a) -> (Kelvin, Kelvin -> a)
down' (Kelvin t, f) = (Kelvin (t - 1), f)

main :: IO ()
main =  do
    let thermostat1 = initialThermostat # up'
    let thermostat2 = thermostat1 # up'
    putStrLn $ toString thermostat2






blocks :: Int -> [a] -> [[a]]
blocks _ [] = []
blocks n ls = la : blocks (2*n) lb
    where (la,lb) = splitAt n ls








data MakeInt a = MakeInt { runMK :: Int -> a }

instance Functor MakeInt where
    fmap f (MakeInt g) = MakeInt (f . g)


cain = do
    let a1  = fmap (+1) (MakeInt (+20))
    let a2 = runMK a1 10
    print a2
