module Exercism.Luhn (isValid) where
import           Data.Char (digitToInt, intToDigit, isDigit)

isValid :: String -> Bool
isValid n = (length normalized >= 2) && (sumDigits normalized `mod` 10 == 0)
  where
    normalized = map digitToInt . reverse . filter isDigit $ n

    sumDigits :: [Int] -> Int
    sumDigits []       = 0
    sumDigits [x]      = x
    sumDigits (x:y:xs) = x + double y + sumDigits xs

    double :: Int -> Int
    double n
      | n < 5     = n * 2
      | otherwise = n * 2 - 9
