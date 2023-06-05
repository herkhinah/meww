module Type where
import Common (Name)

type TypeVar = Int
type MetaVar = Int

data TyBuiltin = PInt | PString | PFloat | PVoid | PPair Ty Ty | PList Ty | PMaybe Ty | PBool deriving (Eq)


data Ty
  = TyData Name [Ty]
  | TyVar TypeVar
  | TyMeta MetaVar
  | TyArrow [Ty] Ty
  | TyBuiltin TyBuiltin
  | TyPoly [TypeVar] Ty
  | TyHole
   deriving (Eq)

instance Show Ty where
  showsPrec :: Int -> Ty -> ShowS
  showsPrec n ty s = showP n ty ++ s
    where
      showP :: Int -> Ty -> String
      showP n (TyVar v) = '"' : show v
      showP n (TyMeta m) = "'" ++ show m
      showP n (TyArrow args res) = showParen (n > 0)
        (\s -> unwords (fmap ((++ " -> ") . showP 1) args) ++ showP 1 res ++ s) ""
      showP n (TyBuiltin builtin) = showsPrec n builtin ""
      showP n (TyPoly vars ty) = showParen (n > 0) (\s -> "forall " ++ unwords (fmap show vars) ++ ". " ++ show ty ++ s) ""

instance Show TyBuiltin where
  showsPrec :: Int -> TyBuiltin -> ShowS
  showsPrec n PInt s = "Int" ++ s
  showsPrec n PString s = "String" ++ s
  showsPrec n PFloat s = "Float" ++ s
  showsPrec n PVoid s = "Void" ++ s
  showsPrec n PBool s = "Bool" ++ s
  showsPrec n (PPair a b) s = showParen (n > 1) ((++) "(" . showsPrec 2 a . showsPrec 2 b) $ ")" ++  s
  showsPrec n (PList a) s = showParen (n > 1) ((++) "[" . showsPrec 2 a) $ "]" ++ s
  showsPrec n (PMaybe a) s = showParen (n > 1) (\s -> "Maybe (" ++ showsPrec 2 a (")" ++ s)) s
