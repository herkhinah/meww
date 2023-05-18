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

data PrimOp
  = Plus
  | Minus
  | Mult
  | Print
  deriving (Show)

data Literal = LitFloat Float | LitInt Int | LitBool Bool | LitString String | LitVoid

instance Show Literal where
  show (LitFloat f) = show f
  show (LitInt i) = show i
  show (LitBool True) = "true"
  show (LitBool False) = "false"
  show (LitString s) = "\"" ++ escape s ++ "\""

escape :: [Char] -> [Char]
escape [] = ""
escape (c : ss) = case c of
  '\\' -> '\\' : escape ss
  '"' -> '\\' : '"' : escape ss
  c -> c : escape ss



data Raw
  = RLam Name Raw
  | RApp Raw Raw
  | RLet Name Raw Raw
  | RVar Name
  | RLiteral Literal 

instance Show Raw where
  show = pShow

pShow (RVar nm) = nm
pShow (RApp rator rand) = let args = foldApp (RApp rator rand) in
  "(" ++ unwords (map pShow args) ++ ")"
  where
    foldApp (RApp rator rand) = foldApp rator ++ [rand]
    foldApp raw = [raw]
pShow (RLet nm def body) = let (defs, raw) = foldLet (RLet nm def body) in
  case defs of
    [(nm, def)] -> "(let (" ++ nm ++ " " ++ pShow def ++ ") " ++ pShow body ++ ")"
    defs -> "let (" ++ unwords (map (\(nm, def) -> "(" ++ nm ++ " " ++ pShow def ++ ")") defs) ++ pShow body ++ ")"
    where
      foldLet :: Raw -> ([(Name, Raw)], Raw)
      foldLet (RLet nm def body) = let (defs, body') = foldLet body in ((nm, def) : defs, body')
      foldLet raw = ([], raw)
pShow (RLam nm raw) = let (nms, raw') = foldLam (RLam nm raw) in
  "(lam (" ++ unwords nms ++ ") " ++ pShow raw' ++ ")"
  where
    foldLam :: Raw -> ([Name], Raw)
    foldLam (RLam nm raw) = let (nms, raw') = foldLam raw in (nm : nms, raw')
    foldLam raw = ([], raw)
pShow (RLiteral lit) = show lit

instance Show Toplevel where
  show (TopFun (Fun {name, args, body})) = "(" ++ name ++ " (" ++ unwords args ++ ") " ++ show body ++ ")"
  show (TopData (Data {name, args, cons})) = "(" ++ name ++ " (" ++ unwords args ++ ") " ++ show cons ++ ")"