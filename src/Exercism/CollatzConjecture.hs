module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n
  | n <= 0    = Nothing
  | otherwise = Just . fst . step $ (0, n)


step :: (Integer, Integer) -> (Integer, Integer)
step (cnt,n)
  | n == 1    = (cnt, 1)
  | odd n     = step (cnt+1, odd' n)
  | otherwise = step (cnt+1, even' n)
  where
    odd' n = 3 * n + 1
    even' n = n `div` 2

