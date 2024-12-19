module WordProblem (answer) where

answer :: String -> Maybe Integer
answer = eval 0 . words . init

eval :: Integer -> [String] -> Maybe Integer
eval n []                       = Just n
eval n ("What":"is":w:ws)       = eval (read w) ws
eval n ("plus":w:ws)            = eval (n + read w) ws
eval n ("minus":w:ws)           = eval (n - read w) ws
eval n ("multiplied":"by":w:ws) = eval (n * read w) ws
eval n ("divided":"by":w:ws)    = eval (n `div` read w) ws
eval _ _                        = Nothing
