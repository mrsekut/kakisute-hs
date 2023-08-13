module SumOfMultiples (sumOfMultiples) where
import           Data.List (nub)


sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum . nub . concatMap (`candidates` limit) $ factors


candidates :: Integer -> Integer -> [Integer]
candidates 0   lim = []
candidates fac lim = takeWhile (< lim) $ map (*fac) [1..]

