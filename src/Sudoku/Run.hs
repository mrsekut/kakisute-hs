-- ref: ヘイヘイHaskell 2.3
--
-- 実行例
-- λ stack ghc -- -main-is Sudoku.Run src/Sudoku/Run.hs -O2 -rtsopts -threaded
-- λ ./src/Sudoku/Run src/Sudoku/sudoku17.1000.txt +RTS -N2 -s
module Sudoku.Run where

import Sudoku.Sudoku
import Control.Exception
import System.Environment
import Control.Parallel.Strategies hiding (parMap)
import Data.Maybe

-- <<main
main :: IO ()
main = do
  [f] <- getArgs
  file <- readFile f

  let puzzles   = lines file
      solutions = runEval (parMap solve puzzles)

  print (length (filter isJust solutions))
-- >>

parMap :: (a -> b) -> [a] -> Eval [b]
parMap f [] = return []
parMap f (a:as) = do
   b <- rpar (f a)
   bs <- parMap f as
   return (b:bs)
