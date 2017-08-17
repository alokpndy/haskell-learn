
{-# LANGUAGE ConstrainedClassMethods #-} -- Allow a class method's type to place additional constraints on a class type variable.
{-# LANGUAGE DeriveGeneric           #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE TypeFamilies            #-}

module TypeFamiliesExtension where

import           GHC.Generics (Generic)


-- Practice TypeFamily

-- Use TypeFamilies when return type of generic functions is ambigious
-- Type synonym family instance are non injective i.e two different type functions can map to same type on RHS
{--
type instance F Int = Bool
type instance F Char = Bool
-- Hence it must be remembered that type checker alone cannot deduce fro RHS.
-- i.e someFunc x :: Bool    will not make it clear if x == Int or x == Char
--}

{-- Data Family are injective
data T Int = T1 Int
data T Char = TC Bool
-- now Thus type checker can infer from LHS as the the RHS is different data constructore
--}


class Grid g where
    type Index g :: *  -- using TypeFamilies feture Associated Type Synonyms
    type Direction g :: *

    indices :: g -> [Index g]

    -- need ConstrainedClassMethod for Eq (g)  i.e constraint on class type variable
    contains :: Eq (Index g) => g -> Index g -> Bool
    contains g a = a `elem` indices g

    distance :: g -> Index g -> Index g -> Int

    minDistance :: g -> [Index g] -> Index g -> Int
    minDistance = defaultMinDistance

    defaultMinDistance :: g -> [Index g] -> Index g -> Int
    defaultMinDistance g xs a = minimum . map (distance g a) $ xs

    -- here we have an associated type i.e Eq (Index g)
    -- Non type-variable argument in the constraint: Eq (Index g) eg. not Eq (a)
    -- (Use FlexibleContexts to permit this)
    -- Also
    -- Constraint ‘Eq (g)’ in the type of ‘neighbours’
    -- constrains only the class type variables --  Use ConstrainedClassMethods to allow it
    neighbours :: Eq (Index g) => g -> Index g -> [Index g]
    neighbours = defaultNeighbours

    defaultNeighbours :: g -> Index g -> [Index g]
    defaultNeighbours g a = filter (\b -> distance g a b == 1 ) $ indices g
--

data UnboundedSquareGrid = UnboundedSquareGrid
  deriving (Eq, Show, Generic)

data SquareDirection = North | East | South | West
   deriving (Show, Eq)

data RectSquareGrid = RectSquareGrid (Int, Int) [(Int, Int)]
    deriving  (Eq, Generic)

instance Show RectSquareGrid where
  show (RectSquareGrid (r,c) _) =
    "rectSquareGrid " ++ show r ++ " " ++ show c

instance Grid RectSquareGrid where
     type Index RectSquareGrid = (Int, Int)
     type Direction RectSquareGrid = SquareDirection
     distance _ (x1, y1) (x2, y2) = abs (x2-x1) + abs (y2-y1)
     indices (RectSquareGrid _ xs) = xs
     contains g (x,y) = 0 <= x && x < c && 0 <= y && y < r
       where
           (r, c) = sizE g
           sizE (RectSquareGrid s _) = s

main :: IO ()
main = do
    let sqGrid = rectSquareGrid 3 5
    let sqInd = indices sqGrid
    let check = contains sqGrid (2,2)
    print sqGrid
    print sqInd
    print check


rectSquareGrid :: Int -> Int -> RectSquareGrid
rectSquareGrid r c =
  RectSquareGrid (r,c) [(x,y) | x <- [0..c-1], y <- [0..r-1]]
