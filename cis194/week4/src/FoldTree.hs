module FoldTree (foldTree, Tree(..)) where

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf
    where insert x Leaf = Node 0 Leaf x Leaf
          insert x (Node h l@Leaf v r) = updateHeight $ Node h (insert x l) v r
          insert x (Node h l v r@Leaf) = updateHeight $ Node h l v (insert x r)
          insert x (Node h l@(Node lh _ _ _) v r@(Node rh _ _ _))
              | lh <= rh  = updateHeight $ Node h (insert x l) v r
              | otherwise = updateHeight $ Node h l v (insert x r)


updateHeight :: Tree a -> Tree a
updateHeight (Node _ l v r) = Node (max (getHeight l) (getHeight r) + 1) l v r
    where getHeight Leaf           = 0
          getHeight (Node h _ _ _) = h
updateHeight l@_       = l

-- height is not just total number inserted!!!
