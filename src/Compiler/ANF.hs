{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Compiler.ANF() where
import Common (Constant)
import Type (Ty(..))
import Data.List (find, findIndex)
import Data.Maybe
import Control.Applicative

data Annot = First | Second

data PrimVal = String String | Float Double | Int Int | Pair PrimVal PrimVal

type AVar = Int

type Name = String

data ANF where
    AV        :: AVar -> ANF
    AApp      :: AVar -> [AVar] -> ANF
    AAppNamed :: Name -> [AVar] -> ANF
    ALet      :: Int -> ANF -> ANF -> ANF
    APrimVal  :: Constant -> ANF
    AFun      :: Name -> ANF
    ACon      :: Name -> ANF
    AConApp   :: Name -> [ANF] -> ANF

data Env = Env {
    locals :: [(Name, (ANF, Ty))],
    cons :: [(Name, (ANF, Ty))],
    funs :: [(Name, (ANF, Ty))]
}

lookupName :: Name -> Env -> ANF
lookupName nm Env { locals, cons } 
    = fromJust ( 
        (findIndex ((nm ==) . fst) locals >>= (Just . AV))
            <|> (find ((==) nm . fst) cons >>= (Just . ACon . fst))
            <|> (find ((==) nm . fst) cons >>= (Just . AFun . fst))
    )
{-
compileToANF :: Env -> Raw -> Ty -> ANF
compileToANF env (RVar nm) ty = lookupName nm env
compileToANF env (RLam nm tm) ty = undefined
compileToANF env (RApp a b) ty = undefined
compileToANF env (RLet defs body) ty = undefined
compileToANF env (RLetRec defs body) ty = undefined
compileToANF env (RConst c) ty = APrimVal c
-}