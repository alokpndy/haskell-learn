module MonadTexcersises  where

import           Control.Applicative
import           Control.Arrow
-- use arrow like - join (***) (+1) (1,2)
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Data.Monoid
import           System.Random


-----------------------------   get   (return, put)
--     StateT  s  m   a   --    s ->  (ma,      s)
foo :: StateT Int IO ()
foo = do
    liftIO $ putStr "Hello enter a value: "   -- IO a -> StateT [Integer] IO a
    ent <- lift getLine                       -- m a -> t m a
    nt <- get
    -- put $ nt + read ent
    modify (+ read ent)-- same as put
    return ()
{-- In Terminal
runStateT foo 3
Hello enter a value: 10
((),13)
--}

pop :: StateT [Integer] IO Integer
pop = do
    liftIO $ putStr "Hello enter a value: "
    s1 <- lift getLine
    (x:xs) <- get
    put xs
    return $ x + read s1
{--
runStateT pop [2]
Hello enter a value: 23
(25,[])
--}


main = do answer <- getStdRandom (randomR (1,100)) -- think of a number
          putStrLn "I'm thinking of a number between 1 and 100, can you guess it?"
          guesses <- execStateT (guessSession answer) 0
          putStrLn $ "Success in " ++ show guesses ++ " tries."

guessSession :: Int -> StateT Int IO ()
guessSession answer =
    do gs <- lift getLine    -- get guess from user
       let g = read gs       -- convert to number
       modify (+1)           -- increment number of guesses
       case compare g answer of
              LT -> do lift $ putStrLn "Too low"
                       guessSession answer
              GT -> do lift $ putStrLn "Too high"
                       guessSession answer
              EQ -> lift $ putStrLn ("Got it!," ++ show answer)


dbError :: MaybeT (ExceptT String (ReaderT () IO) ) Int
dbError = do
    liftIO $ putStr "o"
    return 2
-- runMaybeT ::  ExceptT String (ReaderT () IO) (Maybe Int)
-- runExceptT :: ReaderT () IO (Either String (Maybe Int))
-- runReaderT :: () -> IO (Either String (Maybe Int))
-- Right (Just 2)


{--
StateT s (ExceptT e m) a
has the "expanded" form: s -> m (Either e (s, a))
ExceptT e (StateT s m)
"expands" to: s -> m (s, Either e a).

former allows you to roll-back changes to the state when an error occurs in catchError,

latter commits faulty states making them harder to rollback
--}


rollBack :: StateT String (ExceptT String IO) Int
rollBack = do
    a <- get
    if length a > 5 then lift $ throwE a
                    else return (length a)
-- runStateT :: ::  String -> ExceptT String IO (Int, String)
-- runExceptT (runStateT rollBack "OP") :: :: IO (Either String (Int, String))
-- Just (Right (1,"OP"))

data LengthError = EmptyString
          | StringTooLong Int
          | OtherError String


instance Show LengthError where
    show EmptyString = "The string was empty!"
    show (StringTooLong len) =
        "The length of the string (" ++ show len ++ ") is too long"
    show (OtherError msg) = msg

checkLength :: String -> Either LengthError String
checkLength xs
        | ln == 0 = Left EmptyString
        | ln <= 5 = Left $ OtherError "Not much to deduce"
        | ln > 20 = Left $ StringTooLong ln
        | otherwise = Right xs
        where
            ln = length xs


calculateLength :: ExceptT LengthError IO String
calculateLength = do
     liftIO $ putStrLn "Enter a sentence"
     sen <- lift getLine
     case checkLength sen of
         Left a  -> throwE a
         Right a -> return a

calculateLength' :: ReaderT String (ExceptT LengthError IO) String
calculateLength' = do
    sen <- ask
    case checkLength sen of
        Left a  -> lift $ throwE a
        Right a -> return a
-- runExceptT (runReaderT calculateLength' "I am Alok") --- Right "I am Alok"
-- runExceptT (runReaderT calculateLength' "I am") --- Left Not much to deduce



calculateLength'' :: StateT String (ReaderT String (ExceptT LengthError IO)) String
calculateLength'' = return "kk"

-- runStateT calculateLength'' :: String -> ReaderT String (ExceptT LengthError IO) (String, String)
-- runReaderT (runStateT calculateLength'' "p") :: String -> ExceptT LengthError IO (String, String)
-- runExceptT $ runReaderT (runStateT calculateLength'' "p") "a" :: IO (Either LengthError (String, String)) ---------- Right ("kk","p")



type Env = String
calculateLength''' :: ExceptT LengthError (StateT Int (ReaderT Env IO)) String
calculateLength''' = do
    -- sen <- lift get --- get from StateT
    lift $ modify (+1) -- added as second thougth hence below comments may change
    sen2 <- lift . lift $ ask -- ask ReaderT
    case checkLength sen2 of
        Left a  -> throwE a
        Right a -> return a
-- (runExceptT calculateLength''') :: StateT String (ReaderT Env IO) (Either LengthError String)
-- runStateT (runExceptT calculateLength''') :: String -> ReaderT Env IO (Either LengthError String, String)
--  runReaderT (runStateT (runExceptT calculateLength''')"state") :: Env -> IO (Either LengthError String, String)
-- runReaderT (runStateT (runExceptT calculateLength''')"state") "reader" :: IO (Either LengthError String, String) ---------  (Right "kk","state")



gt :: StateT String IO String
gt = do
   name <- get
   liftIO $ putStrLn "HI"
   put "T"
   return ("hh..." ++ name ++ "...!")
