
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
module Raw.Syntax where

import TT.Name
import Raw.Parser.FC


import Numeric.IEEE (IEEE(identicalIEEE))




data SyntaxInfo = Syn {
    syn_namespace           :: [String]
  , inPattern               :: Bool
  , syn_toplevel            :: Bool
  , typeArrowAllowed    :: Bool
  } deriving (Show)

expandNS :: SyntaxInfo -> Name -> Name
expandNS syn n@(NS _ _) = n
expandNS syn n = case syn_namespace syn of
                        [] -> n
                        xs -> sNS n xs



defaultSyntax :: SyntaxInfo
defaultSyntax = Syn [] False True True


data RecordInfo = RI {
    record_parameters  :: [(Name,PType)]
  , record_constructor :: Name
  , record_projections :: [Name]
  } deriving (Show)



data Const = Str String | I Int | Fl Double | Ch Char | BI Integer
  deriving (Show)

instance Eq Const where
  I i       == I j       = i == j
  Fl i      == Fl j      = identicalIEEE i j
  Ch i      == Ch j      = i == j
  Str i     == Str j     = i == j
  _         == _         = False


data PTerm = PBlock [PBlock]
           | PApp FC PTerm [PTerm]
           | PLam FC Name FC PType PTerm
           | PCase FC PTerm [(PTerm, PTerm)]
           | PConstant FC Const
           | PPatvar FC Name
           | PLet FC Name FC PTerm PType PTerm
           | PTyped PTerm PType
           | PVar FC Name  
           | PAs FC Name PTerm
           | PTrue FC
           | PPair FC PTerm PTerm
           deriving (Show)

data PBlock = BlockExp FC PTerm
            | BlockBind FC Name FC PTerm
            deriving (Show)

data PType = PArrow FC [PType] PType
           | PElided
           | PPlaceholder
           | PRef FC Name [PType]
           | PTypevar FC Name
           | PUnit FC
           | PSigma FC PType PType
           deriving (Show)


