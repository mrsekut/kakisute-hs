module BinarySearch (find) where

import           Data.Array

find :: Ord a => Array Int a -> a -> Maybe Int
find as n = find' ord (bounds as)
  where
    ord i = compare n (as ! i)

find' :: (Int -> Ordering) -> (Int,Int) -> Maybe Int
find' f (si,ei) | si > ei    = Nothing
find' f (si,ei) = case f mi of
  EQ -> Just mi
  LT -> find' f (si,mi-1)
  GT -> find' f (mi+1,ei)
  where
    l = ei - si + 1
    mi = (si + ei) `div` 2
