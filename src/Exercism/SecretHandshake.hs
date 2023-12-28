module Exercism.SecretHandshake where

handshake :: Int -> [String]
handshake = toCommand . toRevBins
  where
    toRevBins 0 = []
    toRevBins n = (n `mod` 2) : toRevBins (n `div` 2)

    toCommand xs@[1,_,_,_,_] = reverse . commands $ xs
    toCommand xs             = commands xs

    commands xs = [action | (bit, action) <- zip xs actions, bit == 1]

    actions = ["wink","double blink","close your eyes","jump"]
