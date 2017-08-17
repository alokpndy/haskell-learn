{-# LANGUAGE TemplateHaskell #-}

module Lens2 where

import           Control.Lens

data Person1 = Person1 { _name1 :: String
                    ,  _age1    :: Double }
                    deriving Show

makeLenses ''Person1

-- name :: Lens' Person1 String
-- age :: Lens' Person1 Double



data Being = Being { _age :: Double } deriving Show
data Person = Person { _personBeing :: Being, _name :: String } deriving Show
data Worker = Worker { _workerPerson :: Person, _job :: String } deriving Show
makeClassy ''Being
makeClassy ''Person
makeClassy ''Worker

-- define manually
instance HasBeing Person where
    being = personBeing
instance HasPerson Worker where
    person = workerPerson
instance HasBeing Worker where
    being = person.being
