{-# LANGUAGE QualifiedDo #-}

module LinearType.Resource where

import qualified Control.Functor.Linear as Control
import Data.Text ( unpack, Text )
import Data.Unrestricted.Linear ( Ur )
import qualified System.IO as System
import qualified System.IO.Resource as Linear


getFirstLine :: FilePath -> Linear.RIO (Ur Text)
getFirstLine fp = Control.do
  handle <- Linear.openFile fp System.ReadMode
  (t, handle') <- Linear.hGetLine handle
  Linear.hClose handle'
  Control.return t


printFirstLine :: FilePath -> System.IO ()
printFirstLine fp = do
  text <- Linear.run (getFirstLine fp)
  System.putStrLn (unpack text)
