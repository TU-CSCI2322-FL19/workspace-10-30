import Data.List
import BOOOl
--data BSTInt = Leaf | Node Int BSTInt BSTInt deriving (Show, Eq)
--data BSTChar = Leaf | Node Char BSTChar BSTChar deriving (Show, Eq)

data BST a = Empty | Node a (BST a) (BST a) deriving Show

values :: BST a -> [a]
values Empty = []
values (Node x lft rgt) = [x] ++ (values lft) ++ (values rgt)

instance Ord a => Eq (BST a) where
  (==) treeA treeB = sort (values treeA) == sort (values treeB)

treeA = Node 7 (Node 3 Empty Empty) (Node 10 Empty Empty)
treeB = Node 3 Empty (Node 7 Empty (Node 10 Empty Empty))
{-
class Eq a where  
    (==) :: a -> a -> Bool  
    (/=) :: a -> a -> Bool  
    x == y = not (x /= y)  
    x /= y = not (x == y)  

-}
