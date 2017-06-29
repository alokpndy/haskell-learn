{-# LANGUAGE OverloadedStrings #-}

module ParseWorks where

import           Control.Applicative
import qualified Data.Attoparsec.Combinator as AC
import           Data.Attoparsec.Text       (Parser)
import qualified Data.Attoparsec.Text       as A
import           Data.Char
import           Data.Text                  (Text, pack)
import qualified Data.Text                  as TI
import qualified Data.Text.IO               as T

{-
DayOfWeek http://michaelxavier.net/posts/2012-01-20-Writing-a-Small-Parser-with-Attoparsec.html
-}

data DayOfWeek = Monday    |
                 Tuesday
                 deriving (Show, Eq)

data DayDate = DayDate Integer  DayOfWeek
     deriving (Show, Eq)

stringChoices :: [Text] -> Parser Text
stringChoices = AC.choice . map A.asciiCI


day :: Parser DayOfWeek
day = monday    <|> tuesday
      where monday = stringChoices ["monday", "mon"] *> pure Monday
            tuesday = stringChoices ["tuesday", "tue"] *> pure Tuesday

dayDate :: Parser DayDate
dayDate = DayDate <$> integer
                  <*> day
        where integer = read <$> A.count 2 A.digit

dateParser :: IO ()
dateParser = do
    file <- T.readFile "booklist.txt"
    print $ A.parseOnly  (many $ many dayDate <* A.endOfLine) file


data Book = Book { bookName, authorName :: Text }
                deriving (Eq, Show)


parseRecord :: Parser (Maybe Book)
parseRecord = do
    trailing <-  unconsumed
    author <- unwords <$> upperCases
    return $ Book <$> bName trailing <*> Just (pack author)

    where
        unconsumed = A.manyTill A.anyChar "by " <* AC.lookAhead (A.satisfy isAsciiUpper)
        upperCases = many bookNames
        reverseWords :: String -> Text
        reverseWords = pack . unwords . reverse . words
        bName :: String -> Maybe Text
        bName xs = case A.parseOnly upperCases (reverseWords xs) of
            Right a -> if length a <= 0 then Nothing
                            else Just ((pack . unwords . reverse) a )
            Left _  ->  Nothing


bookNames = do
    y <- stringChoices [" of the ", " of ", " the ", " ",  ""]
    x <- A.satisfy isAsciiUpper
    xs <- some A.letter
    return $ x : xs ++ TI.unpack y


makeIterator :: Text -> [Either Text (Maybe Book)]
makeIterator xs = case A.parse parseRecord xs of
    A.Fail x _ _  ->  [Left x]
    A.Partial  _  ->  makeIterator "by partialFailed"
    A.Done rest v ->  Right v : makeIterator rest
