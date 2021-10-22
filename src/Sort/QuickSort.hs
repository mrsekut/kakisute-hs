module Sort.QuickSort where


qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort lt ++ [x] ++ qsort gte
  where
    lt  = [ n | n <- xs, n < x ]
    gte = [ n | n <- xs, n >= x ]
