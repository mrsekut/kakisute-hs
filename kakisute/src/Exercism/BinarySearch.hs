module BinarySearch (find) where

import           Data.Array

find :: Ord a => Array Int a -> a -> Maybe Int
find as n = binSearch as n (bounds as)

binSearch :: Ord a => Array Int a -> a -> (Int,Int) -> Maybe Int
binSearch as n (si,ei) | si > ei    = Nothing
binSearch as n (si,ei) = case compare n (as ! mi) of
  EQ -> Just mi
  LT -> binSearch as n (si,mi-1)
  GT -> binSearch as n (mi+1,ei)
  where
    l = ei - si + 1
    mi = (si + ei) `div` 2
