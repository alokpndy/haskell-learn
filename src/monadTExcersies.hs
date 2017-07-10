module MonadTexcersises  where

import           Control.Applicative
import           Control.Arrow
-- use arrow like - join (***) (+1) (1,2)
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State
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
          putStrLn $ "Success in " ++ (show guesses) ++ " tries."

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
              EQ -> lift $ putStrLn "Got it!"
