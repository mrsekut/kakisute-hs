{-# LANGUAGE DeriveGeneric       #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE TypeFamilies        #-}
-- {-# LANGUAGE TypeOperators       #-}

module Generic where

import           GHC.Generics


-- data Bit = I | O
--     deriving (Show, Eq)

-- class Serializable a where
--     serialize :: a -> [Bit]
--     deserialize :: [Bit] -> Maybe (a, [Bit])


-- data Fruit = Apple | Banana | Grape | Orange
--     deriving (Show, Eq)

-- instance Serializable Fruit where
--     serialize Apple  = [I]
--     serialize Banana = [O,I]
--     serialize Grape  = [O,O,I]
--     serialize Orange = [O,O,O,I]
--     deserialize (I:xs)       = Just (Apple, xs)
--     deserialize (O:I:xs)     = Just (Banana, xs)
--     deserialize (O:O:I:xs)   = Just (Grape, xs)
--     deserialize (O:O:O:I:xs) = Just (Orange, xs)
--     deserialize _            = Nothing


-- -- data Tree a = Node (Tree a) (Tree a) | Leaf a
-- --     deriving (Show, Eq)

-- -- instance (Serializable a) => Serializable (Tree a) where
-- --     serialize (Node left right) = [I] ++ serialize left ++ serialize right
-- --     serialize (Leaf a1)         = [O] ++ serialize a1
-- --     deserialize (I:xs) = do (l, remain1) <- deserialize xs
-- --                             (r, remain2) <- deserialize remain1
-- --                             return (Node l r, remain2)
-- --     deserialize (O:xs) = do (x, remain) <- deserialize xs :: Maybe (a, [Bit])
-- --                             return (Leaf x, remain)



-- -- class Generic a where
-- --   type family Rep a :: * -> *    -- representation of the data a
-- --   from :: a -> Rep a x
-- --   to :: Rep a x -> a
-- --   {-# MINIMAL from, to #-}
-- --     -- Defined in ‘HC.Generics

-- data Tree a = Node (Tree a) (Tree a) | Leaf a
--     deriving (Show, Eq, Generic)

-- class Serializable' f where
--     serialize' :: f a -> [Bit]

-- instance Serializable' V1 where
--     serialize' x = undefined

-- instance Serializable' U1 where
--     serialize' x = []

-- instance (Serializable' f, Serializable' g) => Serializable' (f :+: g) where
--     serialize' (L1 x) = I : serialize' x
--     serialize' (R1 x) = O : serialize' x

-- instance (Serializable' f, Serializable' g) => Serializable' (f :*: g) where
--     serialize' (f :*: g) = serialize' f ++ serialize' g

-- instance (Serializable c) => Serializable' (K1 i c) where
--     serialize' (K1 x) = serialize x

-- instance (Serializable' f) => Serializable' (M1 i t f) where
--     serialize' (M1 x) = serialize' x
