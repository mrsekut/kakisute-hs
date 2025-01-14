module Diamond (diamond) where

import           Data.Char (isAlpha, ord, toUpper)

diamond :: Char -> Maybe [String]
diamond c
  | isAlpha c = Just . mirror . map mirror . stairs $ c
  | otherwise = Nothing

stairs :: Char -> [String]
stairs endChar = foldl go [] ['A'..endChar]
  where
    go acc c = map (pad 1 ++) acc ++ [c : pad (length acc)]
    pad n = replicate n ' '

mirror :: [a] -> [a]
mirror xs = init xs ++ reverse xs
