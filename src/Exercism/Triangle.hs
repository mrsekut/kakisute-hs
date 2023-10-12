module Triangle (rows) where

rows :: Int -> [[Integer]]
rows 0 = []
rows 1 = [[1]]
rows n = prows ++ [next (last prows)]
  where
    prows = rows (n-1)


next :: [Integer] -> [Integer]
next [] = [1]
next xs = (++[1]) . (1:) . mid $ xs
  where
    mid (x:y:ys) = x+y : mid (y:ys)
    mid _        = []
