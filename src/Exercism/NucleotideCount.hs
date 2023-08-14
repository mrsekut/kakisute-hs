module DNA (nucleotideCounts, Nucleotide(..)) where

import           Data.Map (Map, fromListWith)

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show, Read)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs = fromList <$> mkNucleotides xs

fromList :: [Nucleotide] -> Map Nucleotide Int
fromList ns = fromListWith (+) $ zip ns [1,1..]

mkNucleotides :: String -> Either String [Nucleotide]
mkNucleotides s
  | isValid s = Right $ map (read . (: [])) s
  | otherwise = Left "invalid"
  where
    isValid = all (`elem` "ACGT")
