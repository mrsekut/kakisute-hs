{-# LANGUAGE OverloadedStrings #-}

module Acronym (abbreviate) where

import qualified Data.Char  as C
import           Data.Maybe (mapMaybe)
import qualified Data.Text  as T

abbreviate :: T.Text -> T.Text
abbreviate xs = T.toUpper . T.pack $ acronyms
  where
    words = splitOnCamelCase =<< splitOnKebab =<< T.words xs
    acronyms = mapMaybe (T.find C.isAlpha) words

splitOnKebab :: T.Text -> [T.Text]
splitOnKebab = T.splitOn "-"

splitOnCamelCase :: T.Text -> [T.Text]
splitOnCamelCase s = if length words == T.length s then [s] else words
  where
    words = splitR C.isUpper s

splitR :: (Char -> Bool) -> T.Text -> [T.Text]
splitR p = reverse . T.foldl part []
  where
    part :: [T.Text] -> Char -> [T.Text]
    part []         c = [T.singleton c]
    part txt@(x:xs) c
      | p c       = T.singleton c : txt
      | otherwise = T.snoc x c : xs

