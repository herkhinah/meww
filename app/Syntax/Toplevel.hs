module Syntax.Toplevel where

import Common (Name, Parse)
import Syntax.Data qualified as D
import Syntax.Term qualified as T
import Text.Megaparsec
import Text.Megaparsec.Char qualified as C
import Text.Megaparsec.Char.Lexer qualified as L
