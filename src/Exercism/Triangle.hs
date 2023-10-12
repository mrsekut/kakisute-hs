module Triangle (rows) where

rows :: Int -> [[Integer]]
rows n = go [] $ replicate n next
  where
    go a []     = []
    go a (f:fs) = f a : go (f a) fs


next :: [Integer] -> [Integer]
next [] = [1]
next xs = (++[1]) . (1:) . mid $ xs
  where
    mid (x:y:ys) = x+y : mid (y:ys)
    mid _        = []
