{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds #-}

module Common(Constant(..), Name, Parser) where

import Data.Void
import Text.Megaparsec
import GHC.Records.Compat ()



type Parser = Parsec Void String

type Name = String

data Constant
    = CBool Bool
    | CFloat Double
    | CString String
    | CInt Int
    | CList [Constant]
    | CUnit

escape :: [Char] -> [Char]
escape [] = ""
escape ('\\' : ss) = '\\' : escape ss
escape ('"' : ss) = '\\' : '"' : escape ss
escape (c : ss) = c : escape ss

instance Show Constant where
  show (CFloat f) = show f
  show (CInt i) = show i
  show (CBool True) = "true"
  show (CBool False) = "false"
  show (CString s) = "\"" ++ escape s ++ "\""
  show _ = undefined
