module Darts (score) where

score :: Float -> Float -> Int
score x y = score' $ distance x y

distance :: Float -> Float -> Float
distance x y = sqrt $ x^2 + y^2

score' :: Float -> Int
score' d
  | d <= 1    = 10
  | d <= 5    = 5
  | d <= 10   = 1
  | otherwise = 0

