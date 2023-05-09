module Syntax.Builtin where

import Common (Parse (parser), Parser)
import Data.Functor
import Text.Megaparsec
import Text.Megaparsec.Char qualified as C
import Text.Megaparsec.Char.Lexer qualified as L

data PrimOp
  = Plus
  | Minus
  | Mult
  | Print
  deriving (Show)

data Literal = Float Float | Int Int | Bool Bool | String String deriving (Show)
