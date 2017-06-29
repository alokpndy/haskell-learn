
module MOnadTMore  where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Maybe

main = do
  password <- runMaybeT getPassword
  case password of
    Just p  -> liftIO $ putStrLn "Password Entered is: "
    Nothing -> liftIO $ putStrLn "invalid password!"

isValid :: String -> Bool
isValid = (>= 10) . length

getPassword :: MaybeT (ExceptT LoginError IO) String
getPassword = do
  password <- liftIO getLine
  guard (isValid password)
  return password


data LoginError = InvalidEmail
              | NoSuchUser
              | WrongPassword
              deriving Show
