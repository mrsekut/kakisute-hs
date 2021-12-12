{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ImportQualifiedPost #-}

module RailwayOriented where

import Data.Text ( Text, toLower )
import Data.Text qualified as T
import Control.Monad ((>=>))


data Request = Request
  { name :: Text
  , email :: Text
  } deriving (Show)



usecase :: Request -> Either Text Request
usecase =
  validate1
  >=> validate2
  >=> validate3
  >=> switch coanonicalizeEmail
  >=> Right . tee updateDB


validate1, validate2, validate3 :: Request -> Either Text Request
validate1 input
  | name input == "" = Left "Name must not be blank"
  | otherwise        = Right input

validate2 input
  | T.length (name input) > 20 = Left "Name must not be longer than 20 chars"
  | otherwise                  = Right input

validate3 input
  | email input == "" = Left "Email must not be blank"
  | otherwise         = Right input


coanonicalizeEmail :: Request -> Request
coanonicalizeEmail input = input { email = toLower $ email input }


updateDB :: Request -> Request
updateDB = id -- dummy



-- Utilities

bind :: (t -> Either f s) -> Either f t -> Either f s
bind f = \case
  Right s -> f s
  Left e -> Left e


tee :: (a -> b) -> a -> a
tee f x = let _ = f x in x

switch :: (t -> b) -> t -> Either a b
switch f = Right . f





-- datas
r1, r2, r3, r4:: Request
r1 = Request { name = "hoge", email = "foo@poyo.com"}
r2 = Request { name = "", email = "Foo@poyo.com"}
r3 = Request { name = "hoge", email = ""}
r4 = Request { name = "hogeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee", email = ""}



-- Example
-- usecase r1      -> Right (Request {name = "hoge", email = "foo@poyo.com"})
-- usecase r4      -> Left "Name must not be longer than 20 chars"