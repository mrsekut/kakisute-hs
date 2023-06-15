{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE RankNTypes #-}
module ST where

import Control.Monad.ST ( ST, runST )
import Data.STRef (newSTRef, modifySTRef, readSTRef, STRef)
import Control.Monad
import Control.Monad.State ( MonadState(get), State, modify, runState )
import GHC.Exts
import GHC.ST
import GHC.Base (liftA2)


st :: ST s Integer
st = do
  n <- newSTRef 0
  forM_ [1..10] $ \i -> do
    modifySTRef n (+i)
  readSTRef n


main :: IO ()
main = print $ runST st   -- 55


r = runST' $ newSTRef 100       -- r :: STRef s Integer

-- runST :: (forall s. ST s a) -> a
runST' :: ST s a -> a
runST' st = undefined

-- newSTRef' :: a -> ST s (STRef b a)
-- newSTRef' = undefined

-- instance Monad (State s) where
--   return a        = State $ \s -> (a,s)
--   (State x) >>= f = State $ \s ->
--     let (a,s')    = x s
--         (State y) = f a
--       in y s'

newtype ST' s a = ST' (STRep' s a)
type STRep' s a = State# s -> (# State# s, a #)

-- | @since 2.01
instance Functor (ST' s) where
    fmap f (ST' m) = ST' $ \ s ->
      case (m s) of { (# new_s, r #) ->
      (# new_s, f r #) }

-- | @since 4.4.0.0
instance Applicative (ST' s) where
    {-# INLINE pure #-}
    {-# INLINE (*>)   #-}
    pure x = ST' (\ s -> (# s, x #))
    m *> k = m >>= \ _ -> k
    (<*>) = ap
    liftA2 = liftM2

instance Monad (ST' s) where
  return a     = ST' $ \s -> (# s, a #)
  (ST' x) >>= f = ST' $ \s ->
    let (# s', a #) = x s
        (ST' y)      = f a
      in y s'