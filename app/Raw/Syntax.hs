module Raw.Syntax where

import Common (Name, Constant (..))
import Type (Ty)

data Cons 
  = Cons Name [Cons]
  | ConsApp Name [Cons]
  | ConsVar Name
  | ConsName Name
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

data Raw
  = RLam [Name] Raw
  | RApp Raw [Raw]
  | RLetRec [(Name, Raw)] Raw
  | RLetRecTyped [(Name, Raw, Ty)] Raw
  | RLet [(Name, Raw)] Raw
  | RLetTyped [(Name, Raw, Ty)] Raw
  | RVar Name
  | RConst Constant 
  | RCase Raw [(Name, [Raw], Raw)]

pattern RBool val = RConst (CBool val)
pattern RString val = RConst (CString val)
pattern RFloat val = RConst (CFloat val)
pattern RInteger val = RConst (CInt val)

instance Show Raw where
  show = pShow

pShow (RVar nm) = nm
pShow (RApp rator args) = "(" ++ pShow rator ++ " " ++ unwords (map pShow args) ++ ")"
pShow (RLet defs body) = "let (" ++ unwords (map (\(nm, def) -> "(" ++ nm ++ " " ++ pShow def ++ ")") defs) ++ pShow body ++ ")"
pShow (RLetRec defs body) = "letrec (" ++ unwords (map (\(nm, def) -> "(" ++ nm ++ " " ++ pShow def ++ ")") defs) ++ pShow body ++ ")"
pShow (RLetTyped defs body) = "let (" ++ unwords (map (\(nm, def, ty) -> "(" ++ nm ++ " : " ++ show ty ++ " " ++ pShow def ++ ")") defs) ++ pShow body ++ ")"
pShow (RLetRecTyped defs body) = "letrec (" ++ unwords (map (\(nm, def, ty) -> "(" ++ nm ++ " : " ++ show ty ++ " " ++ pShow def ++ ")") defs) ++ pShow body ++ ")"
pShow (RLam nms raw) = "(lam (" ++ unwords nms ++ ") " ++ pShow raw ++ ")"
pShow (RConst lit) = show lit

instance Show Toplevel where
  show (TopFun (Fun {name, args, body})) = "(" ++ name ++ " (" ++ unwords args ++ ") " ++ show body ++ ")"
  show (TopData (Data {name, args, cons})) = "(" ++ name ++ " (" ++ unwords args ++ ") " ++ show cons ++ ")"