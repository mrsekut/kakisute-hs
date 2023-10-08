module Minesweeper (annotate) where
import           Data.Char (intToDigit)

data Cell = Mine | Safe deriving (Show, Eq)

-- >>> annotate [" **  * "]
-- ["1**11*1"]
annotate :: [String] -> [String]
annotate xs = a4
  where
    cells = map (map toCell) xs
    a2 = map slide1 cells
    a3 = map (map calc1) a2
    a4 = map (map toChar) a3


toCell :: Char -> Cell
toCell '*' = Mine
toCell  _  = Safe


newtype Cell1 = Cell1 (Cell, [Cell]) deriving Show

-- 隣接する2つの要素を取得
-- これを2次元に応用できることが重要
-- 前後に0があると、N,N+1,N+2を取ったのと同じになる。「後ろの要素を取る」ということを考えなくて済む
-- >>> slide1 [Safe,Safe,Safe,Mine]
-- [Cell1 (Safe,[Safe,Safe]),Cell1 (Safe,[Safe,Safe]),Cell1 (Safe,[Safe,Mine]),Cell1 (Mine,[Safe,Safe])]
slide1 :: [Cell] -> [Cell1]
slide1 = getAdjacents . expand
  where
    expand :: [Cell] -> [Cell]
    expand cs = [Safe] ++ cs ++ [Safe]

    getAdjacents :: [Cell] -> [Cell1]
    getAdjacents (c:c2:c3:cs) = Cell1 (c2,[c,c3]) : getAdjacents (c2:c3:cs)
    getAdjacents _            = []


-- 周り2つをもらって、結果を返す
calc1 :: Cell1 -> (Cell, Int)
calc1 (Cell1 (t, xs)) = (t, (length . filter (Mine ==)) xs)


toChar :: (Cell, Int) -> Char
toChar (Mine, _) = '*'
toChar (Safe, n)
  | n == 0 = ' '
  | otherwise = intToDigit n
