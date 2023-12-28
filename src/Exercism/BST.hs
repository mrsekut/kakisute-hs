module Exercism.BST
    ( BST
    , bstLeft
    , bstRight
    , bstValue
    , empty
    , fromList
    , insert
    , singleton
    , toList
    ) where

data BST a
  = Node (BST a, a, BST a)
  | Empty
  deriving (Eq, Show)

bstLeft :: BST a -> Maybe (BST a)
bstLeft (Node (l,_,_)) = Just l
bstLeft _              = Nothing

bstRight :: BST a -> Maybe (BST a)
bstRight (Node (_,_,r)) = Just r
bstRight _              = Nothing

bstValue :: BST a -> Maybe a
bstValue (Node (_,n,_)) = Just n
bstValue _              = Nothing

empty :: BST a
empty = Empty

fromList :: Ord a => [a] -> BST a
fromList = foldl (flip insert) empty

insert :: Ord a => a -> BST a -> BST a
insert x Empty = singleton x
insert x (Node (l,n,r))
  | x <= n    = Node (insert x l,n,r)
  | otherwise = Node (l,n,insert x r)

singleton :: a -> BST a
singleton x = Node (Empty, x, Empty)

toList :: BST a -> [a]
toList (Node (l,n,r)) = toList l ++ [n] ++ toList r
toList Empty          = []
