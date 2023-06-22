
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
module Raw.Syntax(Fun(..), Raw(..), Pattern(..), Toplevel(..), Data(..)) where

import Common (Name, Constant (..))
import Type (Ty)





--data Data = Data Name [Name] [RCons]

data Data = Data Name [Int] [Ty] deriving (Show)

data Fun = Fun Name [Name] Raw
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

data Pattern
  = PCons Name [Pattern]
  | PBind Name
  | PHole

data Raw
  = RLam [Name] Raw
  | RApp Raw [Raw]
  | RCons Name [Raw]
  | RLetRec [(Name, Raw)] Raw
  | RLetRecTyped [(Name, Raw, Ty)] Raw
  | RLet [(Name, Raw)] Raw
  | RLetTyped [(Name, Raw, Ty)] Raw
  | RVar Name
  | RConst Constant
  | RCase Raw [(Pattern, Raw)]
  | RModule Name Raw
  | RDefine Name Raw
  | RDefineTyped Name Raw Ty
  | RProvide Name
  | RRequire Name
  | RBlock [Raw]
  | RBlockTyped [(Raw, Ty)]



instance Show Raw where
  show = pShow

pShow :: Raw -> String
pShow (RVar nm) = nm
pShow (RApp rator args) = "(" ++ pShow rator ++ " " ++ unwords (map pShow args) ++ ")"
pShow (RLet defs body) = "let (" ++ unwords (map (\(nm, def) -> "(" ++ nm ++ " " ++ pShow def ++ ")") defs) ++ pShow body ++ ")"
pShow (RLetRec defs body) = "letrec (" ++ unwords (map (\(nm, def) -> "(" ++ nm ++ " " ++ pShow def ++ ")") defs) ++ pShow body ++ ")"
pShow (RLetTyped defs body) = "let (" ++ unwords (map (\(nm, def, ty) -> "(" ++ nm ++ " : " ++ show ty ++ " " ++ pShow def ++ ")") defs) ++ pShow body ++ ")"
pShow (RLetRecTyped defs body) = "letrec (" ++ unwords (map (\(nm, def, ty) -> "(" ++ nm ++ " : " ++ show ty ++ " " ++ pShow def ++ ")") defs) ++ pShow body ++ ")"
pShow (RLam nms raw) = "(lam (" ++ unwords nms ++ ") " ++ pShow raw ++ ")"
pShow (RConst lit) = show lit
pShow (RCase _ _) = undefined
pShow (RModule _ _) = undefined
pShow (RRequire _) = undefined
pShow (RProvide _) = undefined
pShow (RDefine _ _) = undefined
pShow (RDefineTyped _ _ _) = undefined
pShow (RBlock _) = undefined
pShow (RBlockTyped _) = undefined
pShow (RCons _ _ ) = undefined

instance Show Toplevel where
  show (TopFun (Fun name args body)) = "(" ++ name ++ " (" ++ unwords args ++ ") " ++ show body ++ ")"
  show (TopData (Data name args cons)) = "(" ++ name ++ " (" ++ unwords (map (\arg -> "'" ++ show arg) args)  ++ ") " ++ show cons ++ ")"