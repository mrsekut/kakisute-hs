{-# LANGUAGE TypeApplications #-}
module BinarySearch (find) where

import           Data.Array


-- >>> find (toArray [1, 3, 4, 6, 8, 9, 11]) 12
-- Nothing
find :: Ord a => Array Int a -> a -> Maybe Int
find as n
  | l == 0    = Nothing
  | n == mid  = Just mi
  | n < mid   = find (sliceArray (si, mi-1) as) n
  | n > mid   = find (sliceArray (mi+1, ei) as) n
  where
    l = length as
    (si,ei) = bounds as
    mi = if even l then (si + ei) `div` 2 else (si + ei + 1) `div` 2
    mid = as ! mi

-- TODO: debug用
-- >>> toArray [1..10]
-- array (0,9) [(0,1),(1,2),(2,3),(3,4),(4,5),(5,6),(6,7),(7,8),(8,9),(9,10)]
toArray :: [Int] -> Array Int Int
toArray xs = listArray @Int (0, length xs-1) xs

-- | 指定された範囲の配列をスライスする関数
-- >>> sliceArray (4,6) (toArray [1..10])
-- array (4,6) [(4,5),(5,6),(6,7)]
sliceArray :: (Ix i) => (i, i) -> Array i e -> Array i e
sliceArray (start, end) arr = array (start, end) [(i, arr ! i) | i <- range (start, end)]


