{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DuplicateRecordFields #-}
-- {-# LANGUAGE NoFieldSelectors #-}

module Record.Record where
import Data.Text (Text, pack)
-- import Prelude hiding (id)

data Foo = Foo { baz :: Int , bar :: Text } deriving (Show)


foo1 :: Foo
foo1 = Foo { baz = 1, bar = pack "John" }


patt :: Foo -> Bool
patt = \case
  Foo { baz = 1 } -> True
  _               -> False


update :: Foo -> Int -> Foo
update foo newId = foo { baz = newId }

-- get :: Foo -> Int
-- get = baz

user1 :: User
user1 = User { id = 1, name = pack "mrsekut"}

data User = User
  { id :: Int
  , name :: Text
  } deriving (Show)

data Item = Item
  { id :: Int
  , name :: Text
  } deriving (Show)


-- update' :: User -> Int -> User
-- update' user newId = user { id = newId }