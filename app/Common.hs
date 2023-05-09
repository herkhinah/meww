module Common (Parser, Parse, Name, parser) where

import Data.Void
import Text.Megaparsec

type Parser = Parsec Void String

class Parse a where
  parser :: Parser a

type Name = String