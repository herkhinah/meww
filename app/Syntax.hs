module Syntax where

import Common (Name)

data ConsArg = ConsName Name | TypeVar Name | ConsApp Name [ConsArg] deriving (Show)

data Cons = Cons
  { name :: Name,
    args :: [ConsArg]
  }
  deriving (Show)

data Data = Data {name :: Name, args :: [Name], cons :: [Cons]} deriving (Show)

data Fun = Fun
  { name :: Name,
    args :: [Name],
    body :: Raw
  }
  deriving (Show)

data Toplevel
  = TopFun Fun
  | TopData Data
  deriving (Show)

data PrimOp
  = Plus
  | Minus
  | Mult
  | Print
  deriving (Show)

data Literal = LitFloat Float | LitInt Int | LitBool Bool | LitString String | LitVoid deriving (Show)

data Raw
  = RLam Name Raw
  | RApp Raw Raw
  | RLet Name Raw Raw
  | RVar Name
  | RLiteral Literal
  | RPrimOp PrimOp
  deriving (Show)
