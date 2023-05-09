module Syntax where

import Common (Name)
import Syntax.Data (Data)
import Syntax.Term (Tm)

newtype Widget = Widget {children :: Widget} deriving (Show)

data Geometry = Geometry
  { x :: String,
    y :: String,
    width :: String,
    heigh :: String,
    anchor :: String
  }
  deriving (Show)

data Window = Window
  { windowtype :: String,
    geometry :: Geometry,
    widget :: Name
  }
  deriving (Show)

data Toplevel
  = TopFun Fun
  | TopData Data
  | TopWindow Window
  | TopWidget Widget
  deriving (Show)

data Fun = Fun
  { name :: Name,
    args :: [Name],
    body :: Tm
  }
  deriving (Show)
