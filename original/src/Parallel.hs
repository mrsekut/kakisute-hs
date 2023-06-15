module Parallel where

import Control.Parallel ()
import Control.Parallel.Strategies ( rpar, rseq, runEval )
import Control.Exception ( evaluate )
import Text.Printf ()
import System.Environment ( getArgs )

-- <<fib
fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
-- >>

-- <<main
main = do
  -- [n] <- getArgs
  -- let test = [test1,test2,test3,test4] !! (read n - 1)
  let test = test1
  r <- evaluate (runEval test)
  print r
-- >>

-- <<test1
test1 = do
  x <- rpar (fib 31)
  y <- rpar (fib 30)
  return (x,y)
-- >>

-- <<test2
test2 = do
  x <- rpar (fib 31)
  y <- rseq (fib 30)
  return (x,y)
-- >>

-- <<test3
test3 = do
  x <- rpar (fib 31)
  y <- rseq (fib 30)
  rseq x
  return (x,y)
-- >>

-- <<test4
test4 = do
  x <- rpar (fib 31)
  y <- rpar (fib 30)
  rseq x
  rseq y
  return (x,y)
-- >>
