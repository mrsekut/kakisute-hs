module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n
  | n <= 0    = Nothing
  | otherwise = Just $ fst (withCount (==1) collatz' n)
  where
    collatz' n
      | n == 1    = 1
      | odd n     = 3 * n + 1
      | otherwise = n `div` 2


withCount :: (a -> Bool) -> (a -> a) -> a -> (Integer, a)
withCount isDone f = go 0
  where
    go cnt y
      | isDone y  = (cnt, y)
      | otherwise = go (cnt+1) (f y)
