{-# LANGUAGE TupleSections #-}
module Minesweeper (annotate) where
import           Data.Char  (intToDigit)
import           Data.List  (find)
import           Data.Maybe (fromMaybe)

data Cell = Mine | Safe deriving (Show, Eq)

-- >>> annotate [ "1" , "*" , "2" , "*" , "1" ]
-- ["1","*","2","*","1"]
annotate xs
  | c == 0 && r==1 = [""]
  |otherwise = a5
  where
    cells = toCells xs

    c = cols cells
    r = length cells

    store' = toCoord cells
    a = map (getAdjacents store') store'
    a3 = map calc1 a
    a4 = map toChar a3
    a5 = chunksOf c a4


-- -- >>> annotate1 [" **  * "]
-- -- ["       ","1**11*1","       "]
-- annotate1 :: [String] -> [String]
-- annotate1 xs = a4
--   where
--     cells = (expand . toCells) xs
--     a2 = map slide1 cells
--     a3 = map (map calc1) a2
--     a4 = map (map toChar) a3

--     -- toCell
--     -- expand

--     -- ↓ここを修正するだけでいいはず
--     -- 1行とるか、3行とるかを指定した関数を実行
--       -- slideして集計していく
--       -- 周りの個数を返す [[ (Cell, Int) ]] ?

--     -- render (toChar)

toCells :: [String] -> [[Cell]]
toCells = map (map toCell)
  where
    toCell :: Char -> Cell
    toCell '*' = Mine
    toCell  _  = Safe


expand :: [[Cell]] -> [[Cell]]
expand rows = [topAndBot] ++ expandRow ++ [topAndBot]
  where
    expandRow = map (\r -> [Safe] ++ r ++ [Safe]) rows
    topAndBot = replicate (cols expandRow) Safe

    cols []     = 0
    cols (c:cs) = length c


-- TODO: name
newtype Cell1 = Cell1 (Cell, [Cell]) deriving Show

-- 隣接する2つの要素を取得
-- これを2次元に応用できることが重要
-- 前後に0があると、N,N+1,N+2を取ったのと同じになる。「後ろの要素を取る」ということを考えなくて済む
-- >>> slide1 [Safe,Safe,Safe,Mine]
-- [Cell1 (Safe,[Safe,Safe]),Cell1 (Safe,[Safe,Mine])]
slide1 :: [Cell] -> [Cell1]
slide1 = getAdjacents
  where
    getAdjacents :: [Cell] -> [Cell1]
    getAdjacents (c:c2:c3:cs) = Cell1 (c2,[c,c3]) : getAdjacents (c2:c3:cs)
    getAdjacents _            = []


-- 周りをもらって、結果を返す
calc1 :: Cell1 -> (Cell, Int)
calc1 (Cell1 (t, xs)) = (t, (length . filter (Mine ==)) xs)


toChar :: (Cell, Int) -> Char
toChar (Mine, _) = '*'
toChar (Safe, n)
  | n == 0 = ' '
  | otherwise = intToDigit n



-- 全てのCellを1次元にし、Indexを振る(?)
-- N-1,N+1, N-+C-+1,という感じでアクセスして抜き出す
-- 和を求める
-- map, map

type Row = Int
type Col = Int
newtype CellWithCoord = CellWithCoord (Cell, Row, Col) deriving Show

type RowCnt = Int
type CellCnt = Int


-- TODO: きれいに内包表記で書けないかな？
-- TODO: あるいは、同じ関数の組み合わせで、row/colともに処理できないかな
byCol :: [a] -> [(a, Int)]
byCol xs = zip xs [1..]
-- byCol xs = [(x,c) | x <- xs, c <- [0..length xs]]

byRow :: [[a]] -> [([a], Int)]
-- byRow xss = [(xs,r) | xs <- xss, r <- [0..length xss]]
byRow xss = zip xss [1..]

byMat :: [[a]] -> [(a, Int, Int)]
-- byMat xss = [(x,r,c) | (xs, r) <- byRow xss, (x,c) <- byCol xs]
byMat xss = bbb
  where
    aaa = byRow xss
    b2 = map g aaa
    bbb = concatMap f b2

    g :: ([a], Int) -> [(a,Int)]
    g (as, r) = map (, r) as

    -- f :: [(a,Int)] -> (a, Int, Int)
    f c = map (\((x,r),c) -> (x,r,c)) a
      where
        a = byCol c



-- TODO: ミスっている気しかしない
toCoord :: [[Cell]] -> [CellWithCoord]
toCoord cells = map toCellWithCoord a
  where
    toCellWithCoord (cell, r, c) = CellWithCoord (cell, r, c)
    a = byMat cells


-- TODO: 1次元だと情報が落ちるので、同じCellに対して複数回加算してしまう
getAdjacents :: [CellWithCoord] -> CellWithCoord -> Cell1
getAdjacents store (CellWithCoord (cell, row, col)) = Cell1 (self, [top1, top2, top3, mid1, mid2, bot1, bot2, bot3])
  where
    self = getFromStore (row, col)

    top1 = getFromStore (row-1, col-1)
    top2 = getFromStore (row-1, col)
    top3 = getFromStore (row-1, col+1)

    mid1 = getFromStore (row, col-1)
    mid2 = getFromStore (row, col+1)

    bot1 = getFromStore (row+1, col-1)
    bot2 = getFromStore (row+1, col)
    bot3 = getFromStore (row+1, col+1)

    getFromStore :: (Int,Int) -> Cell
    getFromStore (row, col) = b
      where
        a = find (\(CellWithCoord (_, row', col')) -> row == row' && col == col') store
        b = maybe Safe (\(CellWithCoord (cell, _, _)) -> cell) a

    (!?) xs i
      | i < 0 = Safe
      | otherwise = case drop i xs of
          (y:ys) -> y
          []     -> Safe


cols :: [[Cell]] -> CellCnt
cols []     = 0
cols (c:cs) = length c


chunksOf :: Int -> [e] -> [[e]]
chunksOf i ls = map (take i) (build (splitter ls)) where
  splitter :: [e] -> ([e] -> a -> a) -> a -> a
  splitter [] _ n = n
  splitter l c n  = l `c` splitter (drop i l) c n

build :: ((a -> [a] -> [a]) -> [a] -> [a]) -> [a]
build g = g (:) []
