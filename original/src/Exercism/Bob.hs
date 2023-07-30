module Bob (responseFor) where
import           Data.Char (isSpace, toUpper)

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
isQuestion xs = last (lastTrim xs) == '?'
  where
    lastTrim :: String -> String
    lastTrim = reverse . startTrim . reverse

    startTrim :: String -> String
    startTrim [] = []
    startTrim (x:xs)
      | isSpace x = startTrim xs
      | otherwise = x:xs


isYell :: String -> Bool
isYell xs = hasString xs && isAllUpper xs
  where
    isAllUpper xs = map toUpper xs == xs
    hasString = any $ flip elem ['A'..'Z']


