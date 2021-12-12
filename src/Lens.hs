{-# LANGUAGE TemplateHaskell #-}

module Lens where

import Control.Lens hiding (element)
import Control.Lens.TH

data Molecule = Molecule { _atoms :: [Atom] } deriving (Show)
data Atom = Atom { _element :: String, _point :: Point } deriving (Show)
data Point = Point { _x :: Double, _y :: Double } deriving (Show)

$(makeLenses ''Molecule)
$(makeLenses ''Atom)
$(makeLenses ''Point)


shiftMoleculeX :: Molecule -> Molecule
shiftMoleculeX = over (atoms . traverse . point . x) (+ 1)

shiftAtomX :: Atom -> Atom
shiftAtomX = over (point . x) (+ 1)

molecule = Molecule { _atoms = [atom1, atom2]}
atom1 = Atom { _element = "C", _point = Point { _x = 1.0, _y = 2.0 } }
atom2 = Atom { _element = "C", _point = Point { _x = 4.0, _y = 6.0 } }