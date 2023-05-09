module Syntax.Term where

import Common (Name, Parse, Parser)
import Syntax.Builtin (Literal, PrimOp)
import Text.Megaparsec
import Text.Megaparsec.Char qualified as C
import Text.Megaparsec.Char.Lexer qualified as L

data Tm
  = Lam [Name] Tm
  | App Tm [Tm]
  | Let Name Tm Tm
  | Var Name
  | List [Tm]
  | Literal Literal
  | PrimOp PrimOp
  | Void
  deriving (Show)
