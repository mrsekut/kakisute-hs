module Exercism.Minesweeper (annotate) where
import           Data.Char (intToDigit)
import           Data.List (find)


data Cell = Mine | Safe deriving (Show, Eq)

annotate :: [String] -> [String]
annotate xs
  | c == 0 && r == 1 = [""]
  | otherwise = chunksOf c $ map (toChar. calcFromAdjs) cellWithAdjs
  where
    cells = toCells xs

    c = cols cells
    r = length cells

    store = toCoord cells
    cellWithAdjs = map (getAdjacents store) store


toCells :: [String] -> [[Cell]]
toCells = map (map toCell)
  where
    toCell :: Char -> Cell
    toCell '*' = Mine
    toCell  _  = Safe


newtype CellWithCoord = CellWithCoord (Cell, Row, Col) deriving Show
type Row = Int
type Col = Int


toCoord :: [[Cell]] -> [CellWithCoord]
toCoord cells = map CellWithCoord (withCoordinates cells)

withCoordinates :: [[a]] -> [(a, Row, Col)]
withCoordinates matrix = [(x, r, c) | (r, row) <- zip [0..] matrix, (c, x) <- zip [0..] row]


newtype CellWithAdjs = CellWithAdjs (Cell, [Cell]) deriving Show

getAdjacents :: [CellWithCoord] -> CellWithCoord -> CellWithAdjs
getAdjacents store (CellWithCoord (cell, row, col)) = CellWithAdjs (self, [top1, top2, top3, mid1, mid2, bot1, bot2, bot3])
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
    getFromStore (row, col) = maybe Safe (\(CellWithCoord (cell, _, _)) -> cell) $ find (\(CellWithCoord (_, row', col')) -> row == row' && col == col') store

    (!?) xs i
      | i < 0 = Safe
      | otherwise = case drop i xs of
          (y:ys) -> y
          []     -> Safe


type CellCnt = Int

cols :: [[Cell]] -> CellCnt
cols []     = 0
cols (c:cs) = length c


calcFromAdjs :: CellWithAdjs -> (Cell, Int)
calcFromAdjs (CellWithAdjs (t, xs)) = (t, (length . filter (Mine ==)) xs)


toChar :: (Cell, Int) -> Char
toChar (Mine, _) = '*'
toChar (Safe, n)
  | n == 0 = ' '
  | otherwise = intToDigit n


chunksOf :: Int -> [e] -> [[e]]
chunksOf _ [] = []
chunksOf n xs
  | n <= 0    = error "Size of chunks should be greater than 0"
  | otherwise = take n xs : chunksOf n (drop n xs)

