module IxStateMonad where

import           Control.Monad.State

bind = (>>=)

test1 :: (Int, Int)
test1 = runState c 0
  where
  c = do
    v <- get
    put $ succ v
    return v


test1' :: (Int, Int)
test1' = runState c 0
  where
  c = get `bind` (
    \v -> put (succ v) `bind` (
      \_ -> return v))







class IxMonad m where
  ireturn :: a -> m p p a
  ibind :: m p q a -> (a -> m q r b) -> m p r b


newtype M m p q a = M { unM:: m a }

instance Monad m => IxMonad (M m) where
  ireturn = M . return
  ibind (M m) f = M (m >>= unM . f)

-- newtype State s a = State { runState :: s -> (a, s) }

iget :: (MonadState s m) => M m s s s
iget = M get

iput :: (MonadState s m) => s -> M m s s ()
iput = M . put


test2 :: (Int, Int)
test2 = runState (unM c) 0
  where
  c = iget `ibind` (
        \v -> iput (succ v) `ibind` (
          \_ -> ireturn v))
-- (0, 1)


-- newtype StateT s m v = StateT { runStateT :: s -> m (v,s) }

-- instance (Monad m) => Monad (StateT s m) where
--   return x = StateT $ \s -> return (x, s)
--   m >>= f  = StateT $ \s -> do
--     (x, sm) <- runStateT m s
--     runStateT (f x) sm

-- get :: (Monad m) => StateT s m s
-- get = state $ \ s -> (s, s)

-- put :: (Monad m) => s -> StateT s m ()
-- put s = state $ const ((), s)


newtype IxStateT m si so v = IxStateT { runIxStateT:: si -> m (v, so) }

instance Monad m => IxMonad (IxStateT m) where
  ireturn x = IxStateT $ \si -> return (x, si)
  ibind (IxStateT m) f = IxStateT $ \si -> do
    (x, sm) <- m si
    runIxStateT (f x) sm

vsget :: Monad m => IxStateT m si si si
vsget = IxStateT (\si -> return (si,si))

vsput :: Monad m => so -> IxStateT m si so ()
vsput s = IxStateT (\_ -> return ((), s))

test4' :: IO ()
test4' = runIxStateT c 0 >>= print
  where
  -- c = do
  --   v <- vsget
  --   vsput $ show $ succ v
  --   return v
  c = vsget `ibind` (
    \v -> vsput (show $ succ v) `ibind` (
      \_ -> ireturn v))
  -- c = vsget `ibind` (
  --   \v -> vsput (show v) `ibind` (
  --     \_ -> vsget `ibind` (
  --       \v' -> ireturn (v,v'))))


test3 :: IO ()
test3 = runIxStateT c 0 >>= print
  where
  c = vsget `ibind` (
    \v -> vsput (succ v) `ibind` (
      \_ -> ireturn v))
-- (0,1)
