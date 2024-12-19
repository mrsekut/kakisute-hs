module Exercism.WordCount (wordCount) where

import           Data.Void
import           Text.Megaparsec      (MonadParsec (lookAhead, try), Parsec,
                                       anySingle, many, parse, some, (<|>))
import           Text.Megaparsec.Char (char, digitChar, letterChar)

import           Data.Char
import           Data.Functor         (($>), (<$>), (<&>))
import qualified Data.MultiSet        as MS

import           Data.Either          (fromRight)
import           Prelude              hiding (char, words)

wordCount :: String -> [(String, Int)]
wordCount = MS.toOccurList . MS.fromList . fromRight [] . parse words "" . map toLower


type Parser = Parsec Void String

words :: Parser [String]
words = many (word <|> (anySingle $> "")) <&> filter (not . null)

word :: Parser String
word = str <|> digit
  where
    str = (:) <$> letterChar <*> many (letterChar <|> try (char '\'' <* lookAhead letterChar))
    digit = some digitChar
