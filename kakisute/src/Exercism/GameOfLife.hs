module GameOfLife (tick) where
import           Data.List (tails, transpose)

tick :: [[Int]] -> [[Int]]
tick xss = map (map conway) $ zipWith zip xss neighbors
  where
    neighbors = zipWith (zipWith (-)) (neighborhoodSum xss) xss

conway :: (Int,Int) -> Int
conway (0,n)
  | n == 3    = 1
  | otherwise = 0
conway (1,n)
  | n == 2    = 1
  | n == 3    = 1
  | otherwise = 0


neighborhoodSum :: [[Int]] -> [[Int]]
neighborhoodSum = rowSum . colSum
  where
    colSum = map windows3Sum
    rowSum = transpose . map  windows3Sum. transpose

windows3Sum :: [Int] -> [Int]
windows3Sum = map sum . windows3 . padding

windows3 :: [a] -> [[a]]
windows3 xs = [ [x, y, z] | (x:y:z:_) <- tails xs ]

padding :: [Int] -> [Int]
padding xs = 0 : xs ++ [0]
