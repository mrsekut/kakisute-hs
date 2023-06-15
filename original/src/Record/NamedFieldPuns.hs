{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}


module Record.NamedFieldPuns where


data Man = Man { name :: String }
data Cat = Cat { name :: String }

evilMagic :: Man -> Cat
evilMagic Man{..} = Cat{..}