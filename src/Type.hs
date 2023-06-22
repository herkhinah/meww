 {-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Type(Ty(.., TPair, TList, TMaybe, TBool, TInt), TyBuiltin(..), Builtin(..)) where
import Common (Name)

type TypeVar = Int
type MetaVar = Int

data TyBuiltin = BInt | BString | BFloat | BVoid | BPair Ty Ty | BList Ty | BTuple [Ty] | BMaybe Ty | BBool deriving (Eq, Show)

data Builtin = BuiltinList Ty | BuiltinTuple [Ty]


data Ty
  = TyData Name [Ty]
  | TyVarS Name
  | TyVar TypeVar
  | TyMeta MetaVar
  | TyArrow [Ty] Ty
  | TyBuiltin TyBuiltin
  | TyPoly [TypeVar] Ty
  | TyHole
   deriving (Eq)

pattern TPair :: Ty -> Ty -> Ty
pattern TPair a b = TyBuiltin (BPair a b)
pattern TList :: Ty -> Ty
pattern TList a = TyBuiltin (BList a)
pattern TMaybe :: Ty -> Ty
pattern TMaybe a = TyBuiltin (BMaybe a)
pattern TBool :: Ty
pattern TBool = TyBuiltin BBool
pattern TInt :: Ty
pattern TInt = TyBuiltin BInt




instance Show Ty where
  showsPrec n ty s = showP n ty ++ s
    where
      showP :: Int -> Ty -> String
      showP _ (TyData nm args) = (showParen (n > 1)) (\s -> nm ++ " " ++ unwords (fmap (showP 2) args) ++ s) "" 
      showP _ TyHole = undefined
      showP _ (TyVarS s) = s
      showP _ (TyVar v) = '"' : show v
      showP _ (TyMeta m) = "'" ++ show m
      showP _ (TyArrow [] _) = undefined 
      showP n (TyArrow [arg] res) = showParen (n > 0) (\s -> showP 1 arg ++ " -> " ++ showP 0 res ++ s) ""
      showP _ (TyArrow (arg : args) res) = showParen (n > 0) (\s -> showP 1 arg ++ " -> " ++ showP 0 (TyArrow args res)) ""
      showP n (TyBuiltin builtin) = showsPrec n builtin ""
      showP n (TyPoly vars ty) = showParen (n > 0) (\s -> "forall " ++ unwords (fmap (("\"" ++ ) . show) vars) ++ ". " ++ show ty ++ s) ""


{-
instance Show TyBuiltin where
  showsPrec _ BInt s = "Int" ++ s
  showsPrec _ BString s = "String" ++ s
  showsPrec _ BFloat s = "Float" ++ s
  showsPrec _ BVoid s = "Void" ++ s
  showsPrec _ BBool s = "Bool" ++ s
  showsPrec n (BPair a b) s = showParen (n > 1) ((++) "(" . showsPrec 2 a . showsPrec 2 b) $ ")" ++  s
  showsPrec n (BList a) s = showParen (n > 1) ((++) "[" . showsPrec 2 a) $ "]" ++ s
  showsPrec n (BMaybe a) s = showParen (n > 1) (\s -> "Maybe (" ++ showsPrec 2 a (")" ++ s)) s
-}