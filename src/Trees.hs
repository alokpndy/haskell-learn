module Trees where

import           Data.Foldable (Foldable, foldMap)
import qualified Data.Foldable as F
import           Data.List     (maximum, tails)
import           Data.Monoid   (mappend, mempty)
import           Data.Tree     (Tree (..), rootLabel, subForest)

import           BSTree


-- Binary Tree
data BTree a = BNode { value :: a
                   , left    :: BTree a
                   , right   :: BTree a }
                   | Leaf
                   deriving Show

showTree = do
    let n1 = BNode { value = 1, left = Leaf, right = Leaf }
    let n2 = BNode { value = 2, left = Leaf, right = Leaf }
    let n3 = BNode { value = 3, left = n1, right = n2 }
    print n3

-- Rose Tree or Multiway Tree (More than two children)
-- Use in HTML DOM

data RoseTree a = RoseTree {  rvalue    :: a
                            , rchildren :: [RoseTree a] }
                            deriving Show

showRoseTree = do
    let n1 = RoseTree { rvalue = 1, rchildren = [] }
    let n2 = RoseTree { rvalue = 2, rchildren = [] }
    let n3 = RoseTree { rvalue = 3, rchildren = [] }
    let n4 = RoseTree { rvalue = 6, rchildren = [n1, n2, n3] }
    print n4



-- Traversing a Tree depth-first
-- implmenting Rose Tree from Data.Tree library
depthFirst :: Tree a -> [a]
depthFirst (Node r forest) = r : concat [depthFirst t | t <- forest]

addAllValue :: Tree Int -> Int
addAllValue (Node r forest) = r + sum [addAllValue t | t <- forest]

someTree :: Tree Int
someTree = r
    where
        r = Node { rootLabel = 0, subForest = [n1,n4] }
        n1 = Node { rootLabel = 1, subForest = [n2,n3] }
        n2 = Node { rootLabel = 2, subForest = [] }
        n3 = Node { rootLabel = 3, subForest = [] }
        n4 = Node { rootLabel = 4, subForest = [] }

mundane = do
    print $ depthFirst someTree
    print $ addAllValue someTree




-- Traversing a tree breadth-first
breadthFirst :: Tree a -> [a]
breadthFirst t = bf [t]
    where bf forest
                | null forest = []
                | otherwise = map rootLabel forest ++
                    bf (concatMap subForest forest)

addBreadthTree :: Tree Int -> Int
addBreadthTree t = sum $ breadthFirst t




-- Implementing Foldable Instance for a tree
-- Data.Tree have own Foldable Instance
instance Foldable RoseTree where
    foldMap f null          = mempty
    foldMap f (RoseTree val xs) = foldr mappend (f val) [foldMap f x | x <- xs]

-- find summ using fold
folded :: Tree Int  -> Int
folded = F.foldr1 (+)



-- Height of a Tree
height :: Tree a -> Int
height (Node val []) = 1
height (Node val xs) = 1 + maximum (map height xs)



-- Binary Search Tree Data Structure
binaryTree = do
    let tree = single 5
    let nodes = [6,4,8,2,9]
    let bst = foldl insert tree nodes
    print bst
    print $ find bst 1
    print $ find bst 2
    print $ valid bst





-- SELF BALANCING TREE -- AVL tree
