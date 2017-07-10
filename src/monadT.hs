{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs     #-}

module TransformerM  where

import           Control.Applicative
import           Control.Arrow
-- use arrow like - join (***) (+1) (1,2)
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Data.Char
import           Data.Foldable
import           Data.Functor
import           Data.Maybe
import           Data.Monoid
import           Data.Traversable
import           Data.Tuple


-- Composing Types
newtype Identity a = Identity { runIdentity :: a }

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)



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




v :: Compose [] Maybe (Compose Maybe [] Integer)
v = Compose [Just (Compose $ Just [1])]

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
        let aimb = join $ fmap (runIdentityT . f) ma
            in IdentityT aimb
        -- or Refactored to
        -- IdentityT $ ma >>= runIdentityT . f

----------------    runIdentityT $ IdentityT (Just 2) >>= return . (+1)


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

--     MaybeT               m                      a
--              ExceptT   e              ma
--                              ReadetT  r ma
bar :: MaybeT ( ExceptT String (ReaderT () IO ) ) Int
bar = return  1


-- EitherT
newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance (Functor m) => Functor (EitherT e m) where
    fmap f (EitherT x) = EitherT $ (fmap . fmap) f x

instance (Applicative m) => Applicative (EitherT e m) where
    pure a = EitherT (pure (pure a))
    (EitherT f) <*> (EitherT y) = EitherT ((<*>) <$> f <*> y) ----- liftA2 (<*>) [Right (+1)] [Right 2]
--
instance (Monad m) => Monad (EitherT e m) where
    return = pure

    (>>=) :: EitherT e m a -> (a -> EitherT e m b) -> EitherT e m b
    (EitherT ema) >>= f =
        EitherT $ do
            v <- ema
            case v of
                Left e  ->  return (Left e)
                Right y ->  runEitherT (f y)




-- | ReaderT   -- ask and return
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

-- Exercise
series :: ReaderT String IO ()
series =  ReaderT putStrLn >> ReaderT (putStrLn . fmap toUpper)





data Person = Person String String
    deriving (Show, Eq)



-- | StateT  -  get and put and return and modify
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





---
-- code :: StateT [Integer] IO ()
-- code = do
--     x <- pop
--     io $ print x
--     y <- pop
--     io $ print y
--     return ()
--
-- pop :: StateT [Integer] IO Integer
-- pop = do
--     (x:xs) <- get
--     put xs
--     return x

io :: IO a -> StateT [Integer] IO a
io = liftIO  $ print 3



-- Exercise


-- main :: IO
main = do
  putStr "Enter userName"
  maybeUserName <- readUserName
  case maybeUserName of
        Nothing -> putStrLn "Invalid user name!"
        Just uName -> do
          putStr "Enter email"
          maybeEmail <- readEmail
          case maybeEmail of
            Nothing -> putStrLn "Invalid email!"
            Just email -> do
              maybePassword <- readPassword
              putStr "Enter password"
              case maybePassword of
                Nothing       -> putStrLn "Invalid Password"
                Just password -> putStrLn $ "Password is " ++ password

--


main' :: IO ()
main' = do
  maybeCreds <- runMaybeT $ do
    usr <- readUserName'
    email <- readEmail'
    pass <- readPassword'
    return (usr, email, pass)
  case maybeCreds of
    Nothing        -> putStrLn "Couldn't login!"
    Just (u, e, p) -> putStrLn $ "Login detail: " ++ u ++ ", " ++ e ++ ", " ++ p
--

readUserName :: IO (Maybe String)
readUserName = do
  str <- getLine
  if length str > 5
    then return $ Just str
    else return Nothing

--
readUserName' :: MaybeT IO String
readUserName' = MaybeT $ do
  putStrLn "Enter useName: "
  str <- getLine
  if length str > 5
    then return $ Just str
    else return Nothing
--

readEmail :: IO (Maybe String)
readEmail = do
  str <- getLine
  if length str > 5
    then return $ Just str
    else return Nothing
--
readEmail' :: MaybeT IO String
readEmail' = MaybeT $ do
  putStrLn "Enter email: "
  str <- getLine
  if length str > 5
    then return $ Just str
    else return Nothing
--

readPassword :: IO (Maybe String)
readPassword = do
  str <- getLine
  if length str > 5
    then return $ Just str
    else return Nothing
--
readPassword' :: MaybeT IO String
readPassword' = MaybeT $ do
  putStrLn "Enter password: "
  str <- getLine
  if length str > 5
    then return $ Just str
    else return Nothing
--
