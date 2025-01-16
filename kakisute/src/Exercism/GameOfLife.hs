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


-- >>> neighborhoodSum [[1,2,3],[4,5,6],[7,8,9]]
-- [[12,21,16],[27,45,33],[24,39,28]]
neighborhoodSum :: [[Int]] -> [[Int]]
neighborhoodSum = map smoothCols . smoothRows
  where
    smoothCols = trips add3 0
    smoothRows = trips (zipWith3 add3) (repeat 0)

-- 1次元リストの各要素について、前後3つを取り出して関数を適用。
-- >>> trips add3 0 [1,2,3,4,5]
-- [3,6,9,12,9]
trips :: (a -> a -> a -> b) -> a -> [a] -> [b]
trips f border = map (\[a,b,c] -> f a b c) . windows3 . padding border

-- >>> padding 0 [1,2,3]
-- [0,1,2,3,0]
padding :: a -> [a] -> [a]
padding border xs = border : xs ++ [border]

-- >>> windows3 [1,2,3,4,5]
-- [[1,2,3],[2,3,4],[3,4,5]]
windows3 :: [a] -> [[a]]
windows3 = filter (\x -> length x >= 3) . map (take 3) . tails

add3 :: Int -> Int -> Int -> Int
add3 a b c = a + b + c
