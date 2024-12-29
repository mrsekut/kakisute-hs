module Raindrops (convert) where

convert :: Int -> String
convert n
  | ppp == "" = show n
  | otherwise = ppp
  where
    ppp = concatMap (uncurry f) [(3, "Pling"), (5, "Plang"), (7, "Plong")]
    f x s = if n `mod` x == 0 then s else ""
