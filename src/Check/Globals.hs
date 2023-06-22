{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Check.Globals where

import qualified Data.Map.Strict as M
import Raw.Syntax (Raw)
import Data.Graph (Vertex)
import Type (Ty (..))
import Control.Lens
import Control.Applicative

data Globals = Globals {
    _funs :: M.Map String (Raw, Ty),
    _conss :: M.Map String Ty,
    ___types :: M.Map String Ty,

    _vertices :: [(String, Vertex)],
    _edges :: [(String, Vertex, [Vertex])]
    }

$(makeLenses ''Globals)


data Scope = Scope {
  _parent :: Maybe Scope,
  _terms :: [(String, (Raw, Ty))],
  _types :: [(String, Ty)]
}

$(makeLenses ''Scope)


(~>) :: [Ty] -> Ty -> Ty
(~>) = TyArrow

infixr 1 ~>

type TypeVar = Int
(.:) :: [TypeVar] -> Ty -> Ty
(.:) = TyPoly

infix 6 .:

pattern TV :: Int -> Ty
pattern TV x = TyVar x

{-
builtin :: Scope
builtin = Scope {
  _parent = Nothing,
  _terms = [
    ("True",  TBool),
    ("False", TBool),

    ("empty", [0] .: TList (TyVar 0)),
    ("cons",  [0] .: [TV 0, TList (TV 0)] ~> TList (TV 0)),
    ("head",  [0] .: [TList (TV 0)] ~> TV 0),
    ("tail", [0] .: [TList (TV 0)] ~> TList (TV 0))
    ("length", [0] .: [TList (TV 0)] ~> TInt),

    ("if",    [0] .: [TBool, TV 0, TV 0] ~> TV 0),
    
    ("pair", [0, 1] .: [TV 0, TV 1] ~> TPair (TV 0) (TV 1)),
    ("fst", [0, 1] .: TPair (TV 0) (TV 1) ~> TV 0),
    ("snd", [0, 1] .: TPair (TV 0) (TV 1) ~> TV 1), 
    
    ("eq", [0] .: [TV 0, TV 0] ~> TBool),

    ("just", [0] .: [TV 0] ~> TMaybe (TV 0)),
    ("nothing", [0] .: TMaybe (TV 0))
  ],
  _types = []
}
-}


type instance Index Scope = String
type instance IxValue Scope = (Raw, Ty)

replaceAt :: String -> ((Raw, Ty) -> (Raw, Ty)) -> Scope -> Scope
replaceAt k f scp =
    case (lookup k) $ view terms scp of
      Nothing ->
          -- Not present in the list; add it
          over parent (\scp' -> fmap (replaceAt k f) scp') scp   
      Just _ ->
          -- Present; replace it
          over terms (replaceFirst k f) scp
          where
            replaceFirst _ _ [] = []
            replaceFirst k f ((k', v') : xs) = if k == k' then [(k', f v')] else (k', v') : (replaceFirst k f xs)


{-
instance At Scope where
  at k f scope = fmap (\v' -> replaceAt k v' scope ) $ f (lookup k (_terms scope))
-}


instance Show Globals where
  show globals = "Globals {\n"
    ++ "  funs = [\n" ++ concatMap (\(nm, (raw, ty)) -> "    ( \n      nm = " ++ nm ++ "\n      def = " ++ show raw ++ "\n      ty = " ++ show ty ++ "\n    ),\n" ) (M.toList (_funs globals)) ++ "  ]"
    ++ "\n}"


