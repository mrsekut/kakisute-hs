module CryptoSquare (encode) where
import           Data.Char (isAlphaNum, isSpace, toLower)
import           Data.List (transpose)

encode :: String -> String
encode = unwords . transpose . split . prettify
  where
    prettify :: String -> String
    prettify = map toLower . filter isAlphaNum

    split :: String -> [String]
    split xs = chunksOf (col xs) xs

    col :: String -> Int
    col = ceiling . sqrt . fromIntegral . length

    chunksOf :: Int -> String -> [String]
    chunksOf n "" = []
    chunksOf n xs = take n (xs ++ repeat ' ') : chunksOf n (drop n xs)
