{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}
-- {-# LANGUAGE DeriveDataTypeable         #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}




module ExtEff where

import           Data.Functor

import           Data.Typeable (Typeable, gcast1)
newtype Id x = Id x

data Union r v where
  Union :: (Functor t, Typeable t) => Id (t v) -> Union r v

instance Functor (Union r) where
  fmap ab (Union (Id x)) = Union (Id (fmap ab x))

data VE w r = Val w | E (Union r (VE w r))

newtype Eff r a = Eff {runEff :: forall w. (a -> VE w r) -> VE w r}

instance Functor (Eff r) where
  fmap f (Eff c) = Eff $ \k -> c (\a -> k (f a))

instance Applicative (Eff r) where
  pure x = Eff $ \k -> k x
  (Eff a) <*> (Eff b) = Eff $ \k -> (a (\ab -> (b (\c -> k (ab c)))))

instance Monad (Eff r) where
  return = pure
  m >>= f = Eff $ \k -> runEff m (\v -> runEff (f v) k)

admin :: Eff r w -> VE w r
admin (Eff m) = m Val

send :: (forall w. (a -> VE w r) -> Union r (VE w r)) -> Eff r a
send f = Eff $ \k -> E (f k)

data Void -- no constructors

run :: Eff Void w -> w
run m = case admin m of Val x -> x

infixr 1 |>

data (a :: * -> *) |> b

decomp :: Typeable t => Union (t |> r) v -> Either (Union r v) (t v)
decomp (Union v) | Just (Id x) <- gcast1 v = Right x
decomp (Union v) = Left (Union v)

handle_relay :: Typeable t => Union (t |> r) v -> (v -> Eff r a) -> (t v -> Eff r a) -> Eff r a
handle_relay u loop h = case decomp u of
  Right x -> h x
  Left u  -> send (\k -> fmap k u) >>= loop

class Member (t :: * -> *) r

instance Member t (t |> r)

instance {-# overlaps #-} Member t r => Member t (t' |> r)

inj :: (Functor t, Typeable t, Member t r) => t v -> Union r v
inj x = Union (Id x)

prj :: (Functor t, Typeable t, Member t r) => Union r v -> Maybe (t v)
prj (Union v) | Just (Id x) <- gcast1 v = Just x
prj _ = Nothing

interpose :: (Typeable t, Functor t, Member t r) => Union r v -> (v -> Eff r a) -> (t v -> Eff r a) -> Eff r a
interpose u loop h = case prj u of
  Just x -> h x
  _      -> send (\k -> fmap k u) >>= loop

newtype Reader e v = Reader (e -> v) deriving (Typeable, Functor)

ask :: (Typeable e, Member (Reader e) r) => Eff r e
ask = send (inj . Reader)

runReader :: Typeable e => Eff (Reader e |> r) w -> e -> Eff r w
runReader m e = loop (admin m)
  where
    loop (Val x) = return x
    loop (E u)   = handle_relay u loop (\(Reader k) -> loop (k e))

local :: (Typeable e, Member (Reader e) r) => (e -> e) -> Eff r a -> Eff r a
local f m = do
  e0 <- ask
  let e = f e0
  let loop (Val x) = return x
      -- data VE w r = Val w | E (Union r (VE w r))
      loop (E u)   = interpose u loop (\(Reader k) -> loop (k e))
  loop (admin m)

newtype Exc e v = Exc e deriving (Typeable, Functor)

throwError :: (Typeable e, Member (Exc e) r) => e -> Eff r a
throwError e = send (\_ -> inj $ Exc e)

runError :: Typeable e => Eff (Exc e |> r) a -> Eff r (Either e a)
runError m = loop (admin m)
  where
    loop (Val x) = return (Right x)
    loop (E u)   = handle_relay u loop (\(Exc e) -> return (Left e))

catchError ::
  (Typeable e, Member (Exc e) r) =>
  Eff r a ->
  (e -> Eff r a) ->
  Eff r a
catchError m handle = loop (admin m)
  where
    loop (Val x) = return x
    loop (E u)   = interpose u loop (\(Exc e) -> handle e)

data Choose v = forall a. Choose [a] (a -> v)

instance Functor Choose where
  fmap f (Choose lst k) = Choose lst (f . k)

choose :: Member Choose r => [a] -> Eff r a
choose lst = send $ (\k -> inj $  Choose lst k)

makeChoice :: forall a r. Eff (Choose |> r) a -> Eff r [a]
makeChoice m = loop (admin m)
  where
    loop (Val x) = return [x]
    loop (E u)   = handle_relay u loop (\(Choose lst k) -> handle lst k)
    handle :: [t] -> (t -> VE a (Choose |> r)) -> Eff r [a]
    handle [] _  = return []
    handle [x] k = loop (k x)
    handle lst k = fmap concat $ mapM (loop . k) lst


data Trace v = Trace String (() -> v)
    deriving (Typeable, Functor)

trace :: Member Trace r => String -> Eff r ()
trace x = send (inj . Trace x)

runTrace :: Eff (Trace |> Void) w -> IO w
runTrace m = loop (admin m)
  where
    loop (Val x) = return x
    loop (E u) = case prj u of
      Just (Trace s k) -> putStrLn s >> loop (k ())
