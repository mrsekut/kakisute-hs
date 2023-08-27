module CryptoSquare (encode) where
import           Data.Char (isAlphaNum, isSpace, toLower)

encode :: String -> String
encode xs = unwords . zipR $ chunk c ys
  where
    ys = prettify xs
    c = col $ length ys

zipR :: [String] -> [String]
zipR [] = []
zipR xs
  | all null xs = []
  | otherwise   = concatMap head' xs : zipR (map tail' xs)
  where
    head' ""     = " "
    head' (s:ss) = [s]

    tail' ""     = ""
    tail' (s:ss) = ss

prettify :: String -> String
prettify = map toLower . filter isAlphaNum

col :: Int -> Int
col l = head [c | c <- [1..], r <- [c-1,c], r * c >= l]

chunk :: Int -> String -> [String]
chunk n "" = []
chunk n xs = take n xs : chunk n (drop n xs)

