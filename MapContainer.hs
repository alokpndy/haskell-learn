module MapContainer where


import           Data.List   as List
import           Data.Map    as Map
import           Data.Monoid
import           Data.Set    as Set
import           Text.Printf

data UUID = UUID
    { uuidPrefix :: Int
    , uuidInfix  :: Int
    , uuidSuffix :: Int
    } deriving (Eq, Ord)

instance Show UUID where
    show (UUID p i s) = printf "%03d-%02d-%04d" p i s

data Gender = Male | Female deriving (Eq, Show)

data Person = Person
    { firstName :: String
    , lastName  :: String
    , gender    :: Gender
    } deriving(Eq)

instance Show Person where
    show (Person fName lName g) = fName ++ ' ':lName ++ " (" ++ show g ++ ")"

type Employees = Map.Map UUID Person


mkUUID :: Int -> Int -> Int -> UUID
mkUUID p i s
  | p <= 0 || p == 666 || p >= 900 = error $ "Invalid SSN prefix: " ++ show p
  | i <= 0 || i > 99 = error $ "Invalid SSN infix: " ++ show i
  | s <= 0 || s > 9999 = error $ "Invalid SSN suffix: " ++ show s
  | otherwise = UUID p i s



employees :: Employees
employees =
    Map.fromList
        [ (mkUUID 525 21 5423, Person "John" "Doe" Male)
        , (mkUUID 521 01 8756, Person "Mary" "Jones" Female)
        , (mkUUID 585 11 1234, Person "William" "Smith" Male)
        , (mkUUID 525 15 5673, Person "Maria" "Gonzalez" Female)
        , (mkUUID 524 34 1234, Person "Bob" "Jones" Male)
        , (mkUUID 522 43 9862, Person "John" "Doe" Male)
        , (mkUUID 527 75 1035, Person "Julia" "Bloom" Female)
        ]


-- look   Map.lookup (UUID 525 21 5423) employees  --Just John Doe (Male)

-- also :  (UUID 525 21 5423) `Map.member` employees -- True

-- finding usinf name - also may be if one is correnct
-- Map.findWithDefault (Person "Bill" "Smith" Male) (UUID 585 11 1234) employees  ------- William Smith (Male)


-- total     Map.size employees

-- deleting    Map.size $ Map.delete (mkSSN 585 11 1234) employees


-- insert    Map.size $ Map.insert (mkSSN 621 24 8736) (Person    "Anthony" "Richardson" Male) employees



--
showMap :: (Show k, Show v) => Map.Map k v -> String
showMap = List.intercalate "\n" . List.map show . Map.toList

main = putStrLn $ showMap employees
