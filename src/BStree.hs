module BSTree () where
-- module BSTree (insert, find, single, valid) where

import           Data.Foldable (Foldable, foldMap)
import qualified Data.Foldable as F
import           Data.List     (tails)
import           Data.Monoid   (mappend, mempty)
import           Data.Tree     (Tree (..), rootLabel, subForest)

data BTree a = BNode {  value :: a
                    , left    :: BTree a
                    , right   :: BTree a}
                | BLeaf
                deriving (Eq, Show)

--
single :: a -> BTree a
single n = BNode n BLeaf BLeaf

--
insert :: Ord a => BTree a -> a -> BTree a
insert (BNode v l r) v'
    | v'< v = BNode v (insert l v') r
    | v' > v = BNode v l (insert r v')
    | otherwise = BNode v l r

insert null v' = BNode v' null null

--
findValue :: Ord a => BTree a -> a -> Bool
findValue (BNode v l r) v'
        | v' < v = findValue l v'
        | v' > v = findValue r v'
        | otherwise = True
findValue null v' = False

--- foldl insert (single 1) [1..5]

main = do
    let n1 = BNode { value = 1, left = BLeaf, right = BLeaf}
    let n2 = BNode { value = 2, left = BLeaf, right = BLeaf}

    let n = BNode { value = 3, left = n1, right = n2}
    print n


-- Multiway or Rose Tree
data RoseTree a = RoseNode { val :: a, children :: [RoseTree a] }
        deriving Show


--
instance Foldable RoseTree where
        foldMap f null = mempty
        foldMap f (RoseNode val xs) = foldr mappend (f val) [foldMap f x | x <- xs]

bush = do
    let n1 = RoseNode { val = 1, children = [] }
    let n2 = RoseNode { val = 2, children = [] }
    let n3 = RoseNode { val = 3, children = [n1, n2] }
    print n3

foldPlus :: RoseTree Int -> Int
foldPlus = F.foldr1 (+)


-- Traversing a tree depth first
mydepthFirst :: RoseTree a -> [a]
mydepthFirst (RoseNode r forest) = r : concat [mydepthFirst n | n <- forest]

addTree :: RoseTree Int -> Int
addTree (RoseNode r forest) = r + sum [addTree n | n <- forest]

addBush = do
    let n1 = RoseNode { val = 1, children = [] }
    let n2 = RoseNode { val = 2, children = [] }
    let n3 = RoseNode { val = 3, children = [n1, n2] }
    let addN = addTree n3
    print (addN, mydepthFirst n3)



adder :: [Int] -> Int
adder []     = 0
adder (x:xs) = x + (adder xs)
