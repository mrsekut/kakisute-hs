module Foldable where


import           Data.Maybe     (fromMaybe)
import           Data.Monoid    (Dual (getDual), Sum (Sum), appEndo)
-- import           Data.Monoid    hiding (Endo)
import           Data.Semigroup (Dual (Dual), Endo (Endo))
import           Prelude        hiding (Foldable, foldMap, foldl, foldr)
import qualified Prelude        as Prelude


class Foldable t where
  fold :: Monoid m => t m -> m
  fold = foldMap id

  foldMap :: Monoid m => (a -> m) -> t a -> m
  foldMap f = foldr (mappend . f) mempty

  foldr :: (a -> b -> b) -> b -> t a -> b
  foldr f z t = appEndo (foldMap (Endo . f) t) z

  foldl :: (a -> b -> a) -> a -> t b -> a
  foldl f z t = appEndo (getDual (foldMap (Dual . Endo . flip f) t)) z

  foldr1 :: (a -> a -> a) -> t a -> a
  foldr1 f xs = fromMaybe (error "foldr1: empty structure")
                  (foldr mf Nothing xs)
    where mf x Nothing  = Just x
          mf x (Just y) = Just (f x y)

  foldl1 :: (a -> a -> a) -> t a -> a
  foldl1 f xs = fromMaybe (error "foldl1: empty structure")
                  (foldl mf Nothing xs)
    where mf Nothing y  = Just y
          mf (Just x) y = Just (f x y)




instance Foldable Maybe where
  foldr _ z Nothing  = z
  foldr f z (Just x) = f x z

  foldl _ z Nothing  = z
  foldl f z (Just x) = f z x

instance Foldable [] where
  foldr = Prelude.foldr
  foldl = Prelude.foldl
  foldr1 = Prelude.foldr1
  foldl1 = Prelude.foldl1

-- instance Ix i => Foldable (Array i) where
--   foldr f z = Prelude.foldr f z . elems




-- mを返す！？
foldMap' :: (Monoid m, Foldable t) => (a -> m) -> t a -> m
foldMap' f = foldr (mappend . f) mempty

mapp :: ([a] -> [b]) -> [[a]] -> [b]
mapp f = foldr ((++) . f) []


fm :: (Monoid m, Foldable t) => t m -> m
fm = foldr mappend mempty




fold' :: (Monoid m, Foldable t) => t m -> m
fold' = foldr mappend mempty

concat' :: [[a]] -> [a]
concat' = foldr (++) []

-- *Foldable> :t (.) mappend
-- (.) mappend :: Monoid a1 => (a2 -> a1) -> (a2 -> a1 -> a1)
-- *Foldable> :t (.)

f :: String -> Bool -> Bool
f = undefined

t :: Foldable a => t a
t = undefined

-- newtype End a = End { appEnd :: a -> a }


-- h :: a1 -> Endo a2
h = Endo . f
