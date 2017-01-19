{-# LANGUAGE GADTs #-}

module GADT where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer
    -- import           Control.Monad.Trans.Reader
import           Data.Functor
import           Data.Monoid
import           Data.Traversable

import           Data.Foldable
import           System.Environment   (getArgs)

import           Data.List            (foldl')
import qualified Data.Map             as Map

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









--- Momization
memoize :: (Int -> a) -> (Int -> a)
memoize f = (map f [0 ..] !!)
