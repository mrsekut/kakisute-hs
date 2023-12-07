module Hamming (distance) where

distance :: String -> String -> Maybe Int
distance xs ys
  | length xs == length ys = Just $ length [() | (x,y) <- zip xs ys, x/=y]
  | otherwise              = Nothing
