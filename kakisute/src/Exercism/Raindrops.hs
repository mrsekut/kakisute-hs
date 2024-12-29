module Raindrops (convert) where

-- TODO: text

-- >>> convert 28
-- >>> convert 30
-- >>> convert 34
-- "Plong"
-- "PlingPlang"
-- "34"
convert :: Int -> String
convert n = if a == "" then show n else a
  where
    a = foldl (\acc f -> acc ++ f n) "" [f3, f5, f7]


f3 :: Int -> String
f3 n = if n `mod` 3 == 0 then "Pling" else ""

f5 :: Int -> String
f5 n = if n `mod` 5 == 0 then "Plang" else ""

f7 :: Int -> String
f7 n = if n `mod` 7 == 0 then "Plong" else ""

other :: Int -> String
other = show
