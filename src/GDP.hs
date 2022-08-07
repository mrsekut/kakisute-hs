-- https://github.com/matt-noonan/gdp/blob/master/src/Theory/Named.hs

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RoleAnnotations #-}

module GDP where

import Data.Coerce ( coerce, Coercible )
import qualified Data.List as L
import Data.Ord ( Down(Down), comparing )



-- Named

newtype Named name a = Named a  -- nameという幽霊型
type role Named nominal nominal

type a ~~ name = Named name a

-- Morally, the type of`name`is
--      a -> (exists name. (a ~~ name))
name :: a -> (forall name. a ~~ name -> t) -> t
name x k = k (coerce x)



-- The

class The d a | d -> a where
  the :: d -> a
  default the :: Coercible d a => d -> a
  the = coerce

instance The (a ~~ name) a



-- SortedBy

newtype SortedBy comp a = SortedBy a    -- compでsort済みであることを表す
instance The (SortedBy comp a) a


sortBy :: ((a -> a -> Ordering) ~~ comp)
       -> [a]
       -> SortedBy comp [a]
sortBy comp xs = coerce (L.sortBy (the comp) xs)


mergeBy :: ((a -> a -> Ordering) ~~ comp)
        -> SortedBy comp [a]
        -> SortedBy comp [a]
        -> SortedBy comp [a]
mergeBy comp xs ys =
        coerce (mergeBy' (the comp) (the xs) (the ys))


mergeBy' :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeBy' comp = go
  where
  go [] ys'          = ys'
  go xs' []          = xs'
  go (x:xs') (y:ys') = case comp x y of
    GT -> y : go (x:xs') ys'
    _  -> x : go xs' (y:ys')


gdpMain :: IO ()
gdpMain = do
  let xs = [1..5]
      ys = reverse [1..5]
  name (comparing Down) $ \gt -> do
    let xs' = sortBy gt xs
        ys' = sortBy gt ys
    print (the (mergeBy gt xs' ys'))



main :: IO ()
main = do
  let xs = [1..5]
      ys = reverse [1..5]
      gt = comparing Down
  print (mergeBy' gt xs ys)


minimum_O1:: SortedBy comp [a]-> Maybe a
minimum_O1 xs = case the xs of
  []    -> Nothing
  (x:_) -> Just x


-- comp指定してないから、miniumの結果5になって違うじゃん
miss :: IO ()
miss = do
  let xs = reverse [1..5]
  name (comparing Down) $ \gt -> do
    let xs' = sortBy gt xs
    print (minimum_O1 xs')
