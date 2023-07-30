-- {-# LANGUAGE DataKinds             #-}
-- {-# LANGUAGE DeriveFunctor         #-}
-- {-# LANGUAGE FlexibleContexts      #-}
-- {-# LANGUAGE FlexibleInstances     #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE TypeFamilies          #-}
-- {-# LANGUAGE TypeSynonymInstances  #-}

module TypeFamily where

import           Prelude                hiding (take)

-- import           Control.Monad.Fix      (fix)
-- import           Control.Monad.Identity
-- import           Data.Finite            (Finite)
-- import           Data.Proxy
-- import qualified Data.Vector.Sized      as V
-- import           GHC.TypeLits



-- class Functor f => Representable f where
--   type Rep f :: *
--   index   :: f a -> Rep f -> a
--   tabulate :: (Rep f -> a) -> f a

--   positions :: f (Rep f)
--   tabulate h = fmap h positions
--   positions  = tabulate id


-- data Stream a = Cons a (Stream a)
--   deriving (Functor, Show, Eq)

-- zeros :: Stream Int
-- -- zeros = Cons 0 zeros
-- zeros = tabulate $ const 0

-- nats :: Stream Int
-- nats = tabulate fromIntegral
-- -- nats = let go n = Cons n (go (n+1)) in go 0


-- instance Representable Stream where
--   type Rep Stream = Integer
--   index (Cons x _) 0  = x
--   index (Cons _ xs) n = index xs (n-1)
--   -- positions = nats
--   positions = fix $ \xs -> Cons 0 (fmap (+1) xs)

--   -- tabulate int2x = go 0
--   --   where go n = Cons (int2x n) (go (n+1))


-- take :: Int -> Stream a  -> [a]
-- take n ~(Cons x xs)
--   | n == 0    = []
--   | n > 0     =  x : take (n - 1) xs
--   | otherwise = error "Stream.take: negative argument."



-- instance Representable ((->) r) where
--   type Rep ((->) r) = r
--   index = id
--   positions = id
--   tabulate = id

-- -- instance Representable Identity where
-- --   type Rep Identity = ()
-- --   index (Identity a) () = a
-- --   tabulate f = Identity (f ())


-- -- f = tabulate (\x -> fromInteger x)
