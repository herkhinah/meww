module Common where

import Data.Void
import Text.Megaparsec

type Parser = Parsec Void String
class Parse a where
  parser :: Parser a

type Name = String

data Constant
    = CBool Bool
    | CFloat Double
    | CString String
    | CInt Int
    | CUnit

escape :: [Char] -> [Char]
escape [] = ""
escape (c : ss) = case c of
  '\\' -> '\\' : escape ss
  '"' -> '\\' : '"' : escape ss
  c -> c : escape ss

instance Show Constant where
  show (CFloat f) = show f
  show (CInt i) = show i
  show (CBool True) = "true"
  show (CBool False) = "false"
  show (CString s) = "\"" ++ escape s ++ "\""
