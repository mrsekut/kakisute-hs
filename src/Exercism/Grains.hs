module Grains (square, total) where

square :: Integer -> Maybe Integer
square n
  | 1 <= n && n <= 64 = Just $ grains !! fromIntegral (n-1)
  | otherwise         = Nothing

total :: Integer
total = sum $ take 64 grains

grains :: [Integer]
grains = iterate (*2) 1
