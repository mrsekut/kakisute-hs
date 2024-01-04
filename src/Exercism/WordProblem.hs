module WordProblem (answer) where
import           Text.Read (readMaybe)

answer :: String -> Maybe Integer
answer problem = eval (0,ws)
  where
    ws = words . init $ problem

eval :: (Integer, [String]) -> Maybe Integer
eval (n,[])                      = Just n
eval (n, "What":"is":w:ws)       = do
  i <- readMaybe w
  eval (i, ws)
eval (n, "plus":w:ws)            = do
  i <- readMaybe w
  eval (n+i, ws)
eval (n, "minus":w:ws)           = do
  i <- readMaybe w
  eval (n-i, ws)
eval (n, "multiplied":"by":w:ws) = do
  i <- readMaybe w
  eval (n*i, ws)
eval (n, "divided":"by":w:ws)    = do
  i <- readMaybe w
  eval (n `div` i, ws)
eval _                           = Nothing
