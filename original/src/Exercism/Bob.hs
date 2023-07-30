module Bob (responseFor) where

import           Data.Char (isAlpha, isSpace, isUpper)
import           Data.List (isSuffixOf)

responseFor :: String -> String
responseFor xs
  | isAllSpace xs              = "Fine. Be that way!"
  | isYell xs && isQuestion xs = "Calm down, I know what I'm doing!"
  | isQuestion xs              = "Sure."
  | isYell xs                  = "Whoa, chill out!"
  | otherwise                  = "Whatever."


isAllSpace :: String -> Bool
isAllSpace = all isSpace


isQuestion :: String -> Bool
isQuestion = isSuffixOf "?" . lastTrim
  where
    lastTrim :: String -> String
    lastTrim = reverse . startTrim . reverse

    startTrim :: String -> String
    startTrim [] = []
    startTrim (x:xs)
      | isSpace x = startTrim xs
      | otherwise = x:xs


isYell :: String -> Bool
isYell xs = hasLetters xs && isAllUpper xs
  where
    isAllUpper = all isUpper . filter isAlpha
    hasLetters = any isAlpha
