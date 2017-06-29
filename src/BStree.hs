module BSTree (insert, find, single, valid) where


data Tree a = Node { value  :: a
                    , left  :: Tree a
                    , right :: Tree a}
                | Leaf
                deriving (Eq, Show)

single :: a -> Tree a
single n = Node n Leaf Leaf

insert :: Ord a => Tree a -> a -> Tree a
insert (Node v l r) v'
    | v' < v = Node v (insert l v') r
    | v' > v = Node v l (insert r v')
    | otherwise = Node v l r
insert _ v' = Node v' Leaf Leaf


find (Node v l r) v'
    | v' < v = find l v'
    | v' > v = find r v'
    | otherwise = True
find Leaf v' = False


-- Verifying the order property of Binary Search Tree
valid :: Ord t => Tree t -> Bool
valid (Node v l r) = leftValid && rightValid
    where leftValid = if notNull l
                        then valid l && value l <= v
                        else True
          rightValid = if notNull r
                         then valid r && v <= value r
                         else True
          notNull t = t /= Leaf
