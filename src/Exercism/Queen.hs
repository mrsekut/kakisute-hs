module Queens (boardString, canAttack) where

import           Data.List       (intersperse)
import           Data.List.Split (chunksOf)


boardString :: Maybe (Int, Int) -> Maybe (Int, Int) -> String
boardString white black = unlines . map (intersperse ' ') . chunksOf 8 $ board
  where
    board = [position (x, y) | x <- [0 .. 7], y <- [0 .. 7]]
    position c
      | Just c == white = 'W'
      | Just c == black = 'B'
      | otherwise       = '_'

canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack (a1,a2) (b1,b2) = case (a1-b1, a2-b2) of
  (0,_) -> True
  (_,0) -> True
  (x,y) -> abs x == abs y
