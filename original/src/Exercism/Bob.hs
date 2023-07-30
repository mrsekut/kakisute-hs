{-# LANGUAGE OverloadedStrings #-}

module Bob (responseFor) where

import           Data.Char (isAlpha)
import           Data.Text (Text)
import qualified Data.Text as T

responseFor :: Text -> Text
responseFor xs
  | isSlience xs                   = "Fine. Be that way!"
  | isShouting xs && isQuestion xs = "Calm down, I know what I'm doing!"
  | isQuestion xs                  = "Sure."
  | isShouting xs                  = "Whoa, chill out!"
  | otherwise                      = "Whatever."


isSlience :: Text -> Bool
isSlience xs = T.length (T.strip xs) == 0


isQuestion :: Text -> Bool
isQuestion = T.isSuffixOf "?" . T.strip


isShouting :: Text -> Bool
isShouting xs = hasLetters xs && isAllUpper xs
  where
    hasLetters = any isAlpha . T.unpack
    isAllUpper xs = T.toUpper xs == xs
