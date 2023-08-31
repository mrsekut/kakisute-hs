module Phone (number) where
import           Data.Char (isDigit)

number :: String -> Maybe String
number xs = checkN =<< checkDigits (filter isDigit xs)
  where
    checkDigits :: String -> Maybe String
    checkDigits xs
      | length xs == 10 = Just xs
      | length xs == 11 = if head xs == '1' then Just $ tail xs else Nothing
      | otherwise       = Nothing

    checkN :: String -> Maybe String
    checkN xs = if is then Just xs else Nothing
      where
        is = all ((> '1') . head) [take 3 xs, drop 3 xs]
