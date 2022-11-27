{-
     5
    /\
   4  3 // node right child and left child
 /
10  leaf 

singelNode is a binTree
-}

module BinaryTree (BinaryTree) where

data BinaryTree a =
    Empty
   | Node (BinaryTree a) a (BinaryTree a)
   | Leaf a 
    deriving (Show)

treeMap :: (a -> b) -> BinaryTree a -> BinaryTree b 
treeMap _ Empty = Empty
-- create leaf node and changed the value
treeMap f (Leaf a) = Leaf (f a)
-- Node contructor  treemap f leftSubt  apply the function on the current value
treeMap f (Node leftSubTree a rightSubTree) = Node (treeMap f leftSubTree) (f a) (treeMap f rightSubTree)

-- make binary tree mappable
instance Functor BinaryTree  where
    fmap = treeMap