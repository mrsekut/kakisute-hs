module Sort.MergeSort where

-- msort :: Ord a => [a] -> [a]
-- msort []  = []
-- msort [x] = [x]
-- msort xs  = merge (msort l) (msort r)
--   where
  -- (l,r) = splitAt (length xs `div` 2) xs

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge xl@(x:xs) yl@(y:ys)
  | x < y     = x : merge xs yl
  | otherwise = y : merge xl ys



msort :: Ord a => [a] -> [a]
msort [] = []
msort xs = go $ map (: []) xs
  where
  go [a] = a
  go xs  = go $ pairs xs

  pairs (a:b:t) = merge a b : pairs t
  pairs t       = t