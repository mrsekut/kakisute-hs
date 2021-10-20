{-# LANGUAGE TypeApplications, DataKinds #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE NamedFieldPuns #-}

module Record.OverloadedLabels where

import GHC.OverloadedLabels
import GHC.Records (HasField (getField))


instance IsLabel "hoge" (Hoge -> Int) where
  fromLabel Hoge { hoge } = hoge

data Hoge = Hoge { hoge :: Int }

g :: Int
g = #hoge (Hoge 1)