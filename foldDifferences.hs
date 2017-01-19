module FoldDiff where


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

foo :: Int -> Int -> Int
foo _ 0 = 0
foo x y = x*y

okk = foldl foo 1 [1,2, undefined,0]
ohh = foldl' foo 1 [1,2, undefined,0] -- exception
