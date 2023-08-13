module DNA (toRNA) where

toRNA :: String -> Either Char String
toRNA = traverse toRNA'
  where
    toRNA' 'G' = pure 'C'
    toRNA' 'C' = pure 'G'
    toRNA' 'T' = pure 'A'
    toRNA' 'A' = pure 'U'
    toRNA' x   = Left x
