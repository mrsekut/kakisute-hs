{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-} {-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module GDP.Head where

import Data.Coerce ( coerce, Coercible )
import qualified Data.List as L
import Data.Ord ( Down(Down), comparing )
import Data.Map (Map)



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



-- Proof

data Proof p = QED deriving (Functor)

-- 値に証明を付ける
newtype a ::: p = SuchThat a

instance The (a ~~ n ::: p) a where
  the (SuchThat x) = the x

(...) :: a -> Proof p -> (a ::: p)
x ... proof = coerce x

data Defn = Defn -- Type exported, constructor hidden.

type Defining f = (Coercible f Defn, Coercible Defn f)

defn :: Defining f => a -> (a ~~ f)
defn = coerce

axiom :: Proof p
axiom = QED



-- List Modules

data IsCons xs
data IsNil  xs
data p == q

-- API functions

gdpHead :: ([a] ~~ xs ::: IsCons xs) -> a
gdpHead xs = head (the xs)

gdpRev :: ([a] ~~ xs) -> ([a] ~~ Reverse xs)
gdpRev xs = defn (reverse (the xs))

length' :: ([a] ~~ xs) -> (Int ~~ Length xs)
length' xs = defn (length (the xs))

-- Names for API functions
newtype Length  xs = Length  Defn
newtype Reverse xs = Reverse Defn

-- Lemmas
rev_length :: Proof (Length (Reverse xs) == Length xs)
rev_length = axiom

rev_rev    :: Proof (Reverse (Reverse xs) == xs)
rev_rev = axiom

rev_cons :: Proof (IsCons xs) -> Proof (IsCons (Reverse xs))
rev_cons _ = axiom

data ListCase a xs = IsCons (Proof (IsCons xs))
                   | IsNil  (Proof (IsNil  xs))

classify :: ([a] ~~ xs) -> ListCase a xs
classify xs = case the xs of
  (_:_) -> IsCons axiom
  []    -> IsNil  axiom


gdpEndpts :: IO (Int, Int)
gdpEndpts = do
  putStrLn "Enter a non-empty list of integers:"
  xs <- readLn
  name xs $ \xs-> case classify xs of
    IsCons proof ->
      return (gdpHead (xs ...proof),
              gdpHead (gdpRev xs ...rev_cons proof))
    IsNil proof -> gdpEndpts
