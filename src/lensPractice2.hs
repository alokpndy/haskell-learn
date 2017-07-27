{-# LANGUAGE KindSignatures #-}
-- This is needed so that we can have constraints in type synonyms.
{-# LANGUAGE RankNTypes     #-}

module LensPractice2 where

import           Control.Applicative
import           Control.Monad.Identity
import           Data.Bifunctor
import           Data.Functor
import           Data.Functor.Compose
import           Data.Functor.Const
