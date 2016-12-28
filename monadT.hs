{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs     #-}

module TransformerM  where

import           Control.Applicative
import           Control.Arrow
-- use arrow like - join (***) (+1) (1,2)
import           Control.Monad
import           Data.Foldable
import           Data.Functor
import           Data.Monoid
import           Data.Traversable
import           Data.Tuple

-- Composing Types
newtype Identity a = Identity { runIdentity :: a }
newtype Compose f g a = Compose { runCompose :: f (g a)}
        deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

-- Functor being closes under composition
-- That is when we compose two functopr we get another functor
-- Compose[ Just (Compose [Just 10])]

-- Aplicative are also closed under composition
instance (Applicative f, Applicative g) => Applicative (Compose f g) where
    pure :: a -> Compose f g a
    pure a = Compose (pure (pure a))

    (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
    (Compose f) <*> (Compose a) =
        Compose $ (<*>) <$> f <*>  a

-- Monad are not closed under composaition


newtype IdentityT f a = IdentityT { runIdentityT :: f a }
    deriving (Eq, Show)

instance (Functor f) => Functor (IdentityT f) where
    fmap f (IdentityT fa) = IdentityT $ fmap f fa

instance (Applicative f) => Applicative (IdentityT f) where
    pure :: a -> IdentityT f a
    pure a = IdentityT (pure a)

    (<*>) :: IdentityT f (a -> b) -> IdentityT f a -> IdentityT f b
    (IdentityT f) <*> (IdentityT a) = IdentityT (f <*> a)

instance (Monad m) => Monad (IdentityT m) where
    return = pure

    (>>=) :: IdentityT m a -> (a -> IdentityT m b) -> IdentityT m b
    (IdentityT ma) >>= f =
        IdentityT $ ma >>= runIdentityT . f




--- Monad Transformer -- PART 2
-- MaybeT

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance (Functor m) => Functor (MaybeT m) where
    fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f ma

instance (Applicative m) => Applicative (MaybeT m) where
    pure a = MaybeT (pure (pure a))

    (<*>) :: MaybeT  m (a -> b) -> MaybeT m a -> MaybeT m b
    (MaybeT f) <*> (MaybeT ma) = MaybeT ((<*>) <$> f <*> ma)

instance (Monad m) => Monad (MaybeT m) where
    return = pure
    (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
    (MaybeT ma) >>= f =
        MaybeT $ do
            v <- ma
            case v of
                Nothing -> return Nothing
                Just y  -> runMaybeT (f y)


-- EitherT
newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance (Functor m) => Functor (EitherT e m) where
    fmap f (EitherT x) = EitherT $ (fmap . fmap) f x

instance (Applicative m) => Applicative (EitherT e m) where
    pure a = EitherT (pure (pure a))
    (EitherT f) <*> (EitherT y) = EitherT ((<*>) <$> f <*> y)
--
-- instance (Monad m) => Monad (EitherT e m) where
--     return = pure
--
--     (EitherT ema) >>= f =
--         EitherT $ do
--             v <- ema
--             case v of
--                 Left e  ->  ema
--                 Right y -> runEitherT ( f y)





-- | ReaderT
newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance (Functor m) => Functor (ReaderT r m) where
    fmap f (ReaderT rma) = ReaderT $ (fmap . fmap) f rma

instance (Applicative m) => Applicative (ReaderT r m) where
    pure a = ReaderT (pure (pure a))
    (ReaderT f) <*> (ReaderT x) =
        ReaderT $ (<*>) <$> f <*> x

instance (Monad m) => Monad (ReaderT r m) where
    return = pure
    (>>=) :: ReaderT r m a -> (a -> ReaderT r m b) -> ReaderT r m b
    (ReaderT rma) >>= f =
        ReaderT $ \r -> do
            a <- rma r
            runReaderT (f a) r


-- | StateT
newtype StateT s m a = StateT { runStateT :: s -> m (a, s)}

instance (Functor m) => Functor (StateT s m) where
    fmap f (StateT sma) = StateT $ (fmap . fmap) (swap . fmap f . swap) sma

instance (Monad m) => Applicative (StateT s m) where
    pure :: a -> StateT s m a
    pure a = StateT $ \s ->   pure (a, s)

    StateT g <*> StateT h = StateT $ \s -> do
                              (f, s') <- g s
                              (x, s'') <- h s'
                              return (f x, s'')


instance (Monad m) => Monad (StateT s m) where
    return  = pure
    (StateT sma) >>= f =
                StateT $ \s -> do
                    (a, s') <- sma s
                    runStateT (f a) s'
