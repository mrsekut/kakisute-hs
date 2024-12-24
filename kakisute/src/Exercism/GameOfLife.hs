module GameOfLife (tick) where
import           Data.List (transpose)

tick :: [[Int]] -> [[Int]]
tick xss = map (map applyRule) $ zipWith zip xss neighbors
  where
    neighbors = zipWith (zipWith (-)) (sumNeighbors xss) xss


-- セルとその周囲のセルの和
sumNeighbors :: [[Int]] -> [[Int]]
sumNeighbors = colSum . rowSum
  where
    colSum = map sumAdjacent
    rowSum = transpose . map sumAdjacent . transpose


sumAdjacent :: [Int] -> [Int]
sumAdjacent = slidingSum . expand
  where
    slidingSum (a:b:c:rs) = (a+b+c) : slidingSum (b:c:rs)
    slidingSum _          = []


expand :: [Int] -> [Int]
expand xs = 0 : xs ++ [0]


applyRule :: (Int,Int) -> Int
applyRule (0,n)
  | n == 3    = 1
  | otherwise = 0
applyRule (1,n)
  | n == 2    = 1
  | n == 3    = 1
  | otherwise = 0
