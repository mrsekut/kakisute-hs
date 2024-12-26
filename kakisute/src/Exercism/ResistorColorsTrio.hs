{-# LANGUAGE OverloadedStrings #-}

module ResistorColorsTrio (Color(..), Resistor(..), label, ohms) where

import           Data.List (find)
import           Data.Text (Text)
import qualified Data.Text as T

data Color =
    Black
  | Brown
  | Red
  | Orange
  | Yellow
  | Green
  | Blue
  | Violet
  | Grey
  | White
  deriving (Show, Enum, Bounded)

newtype Resistor = Resistor { bands :: (Color, Color, Color) }
  deriving Show

label :: Resistor -> Text
label = label' "ohms" . ohms

label' :: Text -> Int -> Text
label' suffix = go suffixes
  where
    go [] n = showNum n <> " " <> suf "giga"
    go (s:ss) n
      | n < 1000 = showNum n <> " " <> suf s
      | otherwise = go ss (n `div` 1000)

    suf s = s <> suffix
    suffixes = ["", "kilo", "mega", "giga"]

showNum :: Int -> Text
showNum  = T.pack . show


ohms :: Resistor -> Int
ohms r = (fromEnum a * 10 + fromEnum b) * 10 ^ fromEnum c
  where
    (a,b,c) = bands r

