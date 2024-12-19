{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}

module Unfold where


import           Prelude hiding (mapM, sequence, sequenceA, traverse)


-- import           Data.List


-- unfold p h t x | p x      = []
--                |otherwise = h x : unfold p h t (t x)

unfold :: (t -> Bool) -> (t -> a) -> (t -> t) -> t -> [a]
unfold p h t x =
  if p x
    then []
    else h x : unfold p h t (t x)


iterate' :: (a -> a) -> a -> [a]
iterate' f = unfoldr $ \x -> Just (x, f x)

repeat' :: a -> [a]
repeat' = unfoldr $ \x -> Just (x, x)



-- Data.List内のものと同じ型のやつ
unfoldr :: (a -> Maybe (b, a)) -> a -> [b]
unfoldr f x = case f x of
  Nothing     -> []
  Just (b, a) -> b : unfoldr f a



map :: (a -> b) -> [a] -> [b]
map f = unfoldr $ \case
  []   -> Nothing
  x:xs -> Just (f x, xs)


-- unfoldr'の双対感を出したfoldr
-- foldr' :: (Maybe (a, b) -> b) -> [a] -> b
-- foldr' f []     = f Nothing
-- foldr' f (x:xs) = f (Just (x, foldr' f xs))

-- sum' :: [Int] -> Int
-- sum' = foldr' $ \case
--   Just (cur, acc) -> cur + acc
--   Nothing         -> 0



-- sum' :: [Int] -> Int
-- sum' = foldr' $ \xx -> case xx of
--   Just (cur, acc) -> cur + acc
--   Nothing         -> 0

int2bin = unfold (==0) (`mod` 2) (`div` 2)
-- int2bin 256 -- > [0,0,0,0,0,0,0,0,1]

map' f =  unfold (==[]) (f.head)  tail

aa = map' (+1)




a = [Nothing, Just 1, Just 2, Just 3]
b = [Just 1, Just 2, Just 3]




-- |Traverceでの定義
-----------------------------
-- traverse :: (Applicative f, Functor t) => (a -> f b) -> t a -> f (t b)
-- traverse f = sequenceA . fmap f

-- sequenceA :: (Applicative f, Functor t) => t (f a) -> f (t a)
-- sequenceA = traverse id

-- sequence :: (Monad m, Functor t) => t (m a) -> m (t a)
-- sequence = sequenceA

-- mapM             :: Monad m => (a -> m b) -> [a] -> m [b]
-- mapM f as        =  sequence (map f as)


-- class (Functor t, Foldable t) => Traversable t where
--   traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
--   traverse f = sequenceA . fmap f

--   mapM :: Monad m => (a -> m b) -> t a -> m (t b)
--   mapM f as = sequence (fmap f as)
--   -- mapM f = unwrapMonad . traverse (WrapMonad . f)


--   sequenceA :: Applicative f => t (f a) -> f (t a)
--   sequenceA = traverse id

--   sequence :: Monad m => t (m a) -> m (t a)
--   sequence = mapM id



-- | 個々の定義 (Listに具体化)
-----------------------------

traverse :: (Applicative f, Functor t) => (a -> f b) -> t a -> f (t b)
traverse f = traverse id . fmap f

-- sequenceA :: Applicative f => [f a] -> f [a]
-- -- sequenceA []     = pure []
-- -- sequenceA (u:us) = (:) <$> u <*> sequenceA us
-- sequenceA = foldr (\u v -> (:) <$> u <*> v) (pure [])




-- sequence :: (Foldable t, Monad m) => t (m a) -> m [a]
-- sequence = foldr k (return [])
--   where
--     k m m' = do { x <- m; xs <- m'; return (x:xs) }



-- f :: [Maybe a] -> [a]
-- f = concat . sequenceA

-- -- g :: [Maybe a] -> [a]
-- t :: (a -> [b]) -> Maybe a -> [Maybe b]
-- t = traverse

-- h :: Maybe a -> [b0]
-- h = undefined
