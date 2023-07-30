{-# LANGUAGE OverloadedStrings #-}

module Bob (responseFor) where

import           Data.Char (isAlpha)
import           Data.Text (Text)
import qualified Data.Text as T

responseFor :: Text -> Text
responseFor xs
  | silience             = "Fine. Be that way!"
  | shouting && question = "Calm down, I know what I'm doing!"
  | question             = "Sure."
  | shouting             = "Whoa, chill out!"
  | otherwise            = "Whatever."
  where
    silence = T.length (T.strip xs) == 0
    question = T.isSuffixOf "?" . T.strip $ xs
    shouting = hasLetters xs && isAllUpper xs
      where
        hasLetters = any isAlpha . T.unpack
        isAllUpper xs = T.toUpper xs == xs
