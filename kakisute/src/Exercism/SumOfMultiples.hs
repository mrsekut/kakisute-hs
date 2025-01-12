module SumOfMultiples (sumOfMultiples) where
import           Data.List (nub)

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum . nub . concatMap (takeWhile (< limit) . muls) $ factors
  where
    muls 0 = [0]
    muls n = map (* n) [1..]
