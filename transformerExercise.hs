
{-# LANGUAGE OverloadedStrings #-}

module TransformerExercise  where

import           Control.Applicative
import           Control.Monad
import           Data.Foldable
import           Data.Functor
import           Data.Map                   as Map
import           Data.Monoid
import           Data.Text
import qualified Data.Text.IO               as T
import           Data.Traversable

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader


embeded :: MaybeT (ExceptT String (ReaderT () IO)) Int
embeded = return 1

maybeUnwrap :: ExceptT String (ReaderT () IO) (Maybe Int)
maybeUnwrap = runMaybeT embeded

eitherUnwrap :: ReaderT () IO (Either String (Maybe Int))
eitherUnwrap = runExceptT maybeUnwrap

readerUnwrap ::() -> IO (Either String (Maybe Int))
readerUnwrap = runReaderT eitherUnwrap -- Right(Just 1)

-- base monad means the outermost monad



-- | Lift
-- class MonadTrans t where
--      lift :: (Monad m) => m a -> t m a

-- class (Monad m) => MonadReader r m | m -> r where
--     ask   :: m r
--     local :: (r -> r) -> m a -> m a
