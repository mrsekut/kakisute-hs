module Diamond (diamond) where

import           Data.Char (isAlpha, ord, toUpper)

diamond :: Char -> Maybe [String]
diamond c
  | isAlpha c = Just . mirror . map mirror . triangleTopLeft $ c
  | otherwise = Nothing

triangleTopLeft :: Char -> [String]
triangleTopLeft x = foldl go [] ['A'..x]
  where
    go rs c = map (expands 1 0) rs ++ [expands 0 (length rs) [c]]
    expands n m t = replicate n ' ' <> t <> replicate m ' '

mirror :: [a] -> [a]
mirror xs = init xs ++ reverse xs
