{-# LANGUAGE LambdaCase #-}

module Traverse where


import           Control.Applicative (WrappedMonad (WrapMonad), unwrapMonad)
import           Control.Monad       (join)
-- import           Prelude             hiding (mapM, sequence, sequenceA,
--                                       traverse)


-- class (Functor t, Foldable t) => Traversable t where
--   traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
--   traverse f = sequenceA . fmap f

--   mapM :: Monad m => (a -> m b) -> t a -> m (t b)
--   -- mapM f = unwrapMonad . traverse (WrapMonad . f)
--   mapM f as = sequence $ fmap f as



--   sequenceA :: Applicative f => t (f a) -> f (t a)
--   sequenceA = traverse id

--   sequence :: Monad m => t (m a) -> m (t a)
--   sequence = mapM id


-- traverseM :: Applicative f => (t -> f a) -> Maybe t -> f (Maybe a)
-- traverseM _ Nothing  = pure Nothing
-- traverseM f (Just x) = Just <$> f x


-- traverseL :: Applicative f => (a -> f b) -> [a] -> f [b]
-- traverseL f = foldr (\x ys -> (:) <$> f x <*> ys) (pure [])


-- traverseL' :: Applicative f => (a -> f b) -> [a] -> f [b]
-- traverseL' f []     = pure []
-- traverseL' f (x:xs) = (:) <$> f x <*> traverseL f xs

-- traverseE :: Applicative f => (t -> f b) -> Either a t -> f (Either a b)
-- traverseE _ (Left x)  = pure (Left x)
-- traverseE f (Right y) = Right <$> f y





--         -- Defined in Data.Traversable
-- instance Traversable [] -- Defined in Data.Traversable
-- instance Traversable Maybe -- Defined in Data.Traversable



-- joinMaybeA :: (Applicative f) => Maybe (f (Maybe b)) -> f (Maybe b)
-- joinMaybeA f = join <$> a
--   where
--     a = Prelude.sequenceA f



-- | 個々の定義 (Listに具体化)
-----------------------------

-- traverse :: (Applicative f, Functor t) => (a -> f b) -> t a -> f (t b)
-- traverse f = traverse id . fmap f

-- sequenceA :: Applicative f => [f a] -> f [a]
-- -- sequenceA []     = pure []
-- -- sequenceA (u:us) = (:) <$> u <*> sequenceA us
-- sequenceA = foldr (\u v -> (:) <$> u <*> v) (pure [])




-- sequence :: (Foldable t, Monad m) => t (m a) -> m [a]
-- sequence = foldr k (return [])
--   where
--     k m m' = do { x <- m; xs <- m'; return (x:xs) }


a = [Nothing, Just 1, Just 2, Just 3]
b = [Just 1, Just 2, Just 3]




-- f :: [Maybe a] -> [a]
-- f = concat . sequenceA
