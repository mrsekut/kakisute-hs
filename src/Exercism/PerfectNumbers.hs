module Exercism.PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify n
  | n <= 0     = Nothing
  | n < n'    = Just Abundant
  | n > n'    = Just Deficient
  | otherwise = Just Perfect
  where
    n' = sum (divisors n) - n
    divisors n = lowers ++ uppers

    lowers = [ x | x <- [1..isqrt n], n `mod` x == 0 ]
    uppers = [ n `div` x | x <- lowers, x /= n `div` x ]

    isqrt = floor . sqrt . fromIntegral

