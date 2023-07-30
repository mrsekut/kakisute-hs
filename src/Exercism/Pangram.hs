-- https://exercism.org/tracks/haskell/exercises/pangram
module Exercism.Pangram (isPangram) where
import           Data.Char (toLower)


isPangram :: String -> Bool
isPangram text = all (`elem` map toLower text) ['a'..'z']
