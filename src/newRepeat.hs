{-# LANGUAGE GADTs #-}

module Newrepeat where


type Option = String

data Config = MakeConfig [Option]
        deriving Show


configBuilder :: [Option] -> Config
configBuilder = MakeConfig

defaultConfig :: [Option] -> Config
defaultConfig options = MakeConfig $ "-Wall" : options



profile :: ([Option] -> Config) -> Config
profile builder = builder ["-prof", "-auto-all"]

---


profile' :: ([Option] -> Config) -> ([Option] -> Config)
profile' builder = \options -> builder $ ["-prof", "-auto-all"] ++ options


goFaster :: ([Option] -> Config) -> ([Option] -> Config)
goFaster builder = \options -> builder $ ["-O2"] ++ options


extract :: ([Option] -> Config) ->  Config
extract builder = builder []



-- (&) is another popular name for this operator
(#) :: a -> (a -> b) -> b
x # f = f x

infixl 0 #

mockery :: IO ()
mockery = do
    let builder1 = defaultConfig # profile'
    let builder2 = builder1 # goFaster
    print (builder2 # extract)



newtype Kelvin = Kelvin { getKelvin :: Double }

newtype Celsius = Celsius { getCelsius :: Double }
    deriving (Show)

kelvinToCelsius :: Kelvin -> Celsius
kelvinToCelsius (Kelvin t) = Celsius (t - 273.15)

initialThermostat :: (Kelvin, Kelvin -> Celsius)
initialThermostat = (Kelvin 298.15, kelvinToCelsius)

extractT :: (Kelvin, Kelvin -> a) -> a
extractT (t, f) = f t

up' :: (Kelvin, Kelvin -> a) -> (Kelvin, Kelvin -> a)
up' (t, f) = (t + 1, f)

down' :: (Kelvin, Kelvin -> a) -> (Kelvin, Kelvin -> a)
down' (t, f) = (t - 1, f)
