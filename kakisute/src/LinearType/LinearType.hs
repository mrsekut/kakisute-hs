{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}

module LinearType.LinearType where
import Prelude.Linear (Ur(..))

-- use Ur
f :: Ur a %1 -> (a, a)
f (Ur a) = (a, a)
