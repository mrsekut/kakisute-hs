module Exercism.NucleotideCount (nucleotideCounts, Nucleotide(..)) where

import           Data.Map (Map, fromListWith)

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs = fromList <$> traverse mkNucleotide xs

fromList :: [Nucleotide] -> Map Nucleotide Int
fromList ns = fromListWith (+) $ zip ns [1,1..]

mkNucleotide :: Char -> Either String Nucleotide
mkNucleotide 'A' = Right A
mkNucleotide 'C' = Right C
mkNucleotide 'G' = Right G
mkNucleotide 'T' = Right T
mkNucleotide _   = Left "invalid"
