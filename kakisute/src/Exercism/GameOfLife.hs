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
neighborhoodSum = map smoothCols . smoothRows
  where
    smoothCols = map (\[a,b,c] -> add3 a b c) . windows3 . padding 0
    smoothRows = map (\[a,b,c] -> zipWith3 add3 a b c) . windows3 . padding [0,0..]
    -- smoothRows = transpose . map smoothCols . transpose

padding :: a -> [a] -> [a]
padding border xs = border : xs ++ [border]

windows3 :: [a] -> [[a]]
windows3 xs = [ [x, y, z] | (x:y:z:_) <- tails xs ]

add3 :: Int -> Int -> Int -> Int
add3 a b c = a + b + c
