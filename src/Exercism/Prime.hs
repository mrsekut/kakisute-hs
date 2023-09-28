module Prime (nth) where

nth :: Int -> Maybe Integer
nth 0 = Nothing
nth n = Just $ filter isPrime [2..] !! (n-1)


isPrime :: Integer -> Bool
isPrime n
  | n < 2     = False
  | n == 2    = True
  | even n    = False
  | otherwise = all (\i -> n `mod` i /= 0) [2..isqrt n]
  where
    isqrt = floor . sqrt . fromIntegral

