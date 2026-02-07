module Exercism.StateOfTicTacToe (gameState, GameState(..)) where
import           Data.List (transpose)

data GameState = WinX | WinO | Draw | Ongoing | Impossible deriving (Eq, Show)

gameState :: [String] -> GameState
gameState board
  | isImpossible board  = Impossible
  | isWinX board        = WinX
  | isWinO board        = WinO
  | isDraw board        = Draw
  | otherwise           = Ongoing

isWin :: Char -> [String] -> Bool
isWin p board = row board || col board || diag1 board || diag2 board
  where
    row = elem [p,p,p]
    col = row . rotate90

    diag1 bs = all (== p) $ zipWith (!!) bs [0,1,2]
    diag2 = diag1 . rotate90

    rotate90 = map reverse . transpose

isWinX,isWinO :: [String] -> Bool
isWinX = isWin 'X'
isWinO = isWin 'O'

isImpossible :: [String] -> Bool
isImpossible board = (isWinX board && isWinO board)
                  || (xc - oc > 1 || oc > xc)
  where
    xc = countX board
    oc = countO board

isDraw :: [String] -> Bool
isDraw board = count board == 9

count,countX,countO :: [String] -> Int
count board = countO board + countX board
countX = length . filter (== 'X') . concat
countO = length . filter (== 'O') . concat
