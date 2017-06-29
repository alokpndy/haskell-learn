-- {-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}


module GentleIntro  where

import           Control.Applicative
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader
import           Data.Map                   as Map
import           Data.Monoid
import           Data.Text
import qualified Data.Text.IO               as T


data LoginError = InvalidEmail
                | NoSuchUser
                | WrongPassword
                deriving Show


users :: Map Text Text
users = Map.fromList [("example.com", "qwerty123"), ("localhost", "password")]

getDomain :: Text -> Either LoginError Text
getDomain email =
  case splitOn "@" email of
    [name, domain] -> Right domain
    _              -> Left InvalidEmail


getToken :: ExceptT LoginError IO Text
getToken = do
    lift (T.putStrLn "Enter email")
    input <- lift T.getLine
    liftEither (getDomain input)

liftEither :: Monad m => Either e a -> ExceptT e m a
liftEither x = ExceptT (return x)

----
printError :: LoginError -> ExceptT LoginError IO a
printError err = do
    lift . T.putStrLn $ case err of
        WrongPassword -> "Wromg Password, get lost"
        NoSuchUser    -> "No user with email exixt"
        InvalidEmail  -> "Invalid emial address entered"
    throwE err

userLogin :: ExceptT LoginError IO Text
userLogin = do
    token <- getToken
    userpw <- maybe (throwE NoSuchUser)
                return (Map.lookup token users)
    password <- lift (T.putStrLn "Enter your pass:" >> T.getLine)

    if userpw == password
        then return token
        else throwE WrongPassword


loginDialogue :: ExceptT LoginError IO ()
loginDialogue = do
  let retry =  userLogin `catchE` wrongPasswordHandler
  token     <- retry `catchE` printError
  lift $ T.putStrLn (append "Logged in with token: " token)

wrongPasswordHandler :: LoginError -> ExceptT LoginError IO Text
wrongPasswordHandler WrongPassword = do
  lift (T.putStrLn "Wrong password, one more chance.")
  userLogin
wrongPasswordHandler err = throwE err


main :: IO ()
main = do
  runExceptT loginDialogue
  return ()
