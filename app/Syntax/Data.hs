module Syntax.Data where

import Common (Name)
import Text.Megaparsec
import Text.Megaparsec.Char qualified as C
import Text.Megaparsec.Char.Lexer qualified as L

{-
(data List ('a')
  Nil
  (Cons (List 'a')))
-}

data ConsArg = Con Name | TypeVar Name | App Name [ConsArg] deriving (Show)

data Cons = Cons
  { name :: Name,
    args :: [ConsArg]
  }
  deriving (Show)

data Data = Data {name :: Name, args :: [Name], cons :: [Cons]} deriving (Show)
