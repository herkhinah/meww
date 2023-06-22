{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use if" #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Check.Check(mkLens, declareToplevelFuns, typecheckToplevelFuns) where

import Raw.Syntax

import Prelude


import Common (Name, Constant (..))
import Control.Applicative ((<|>))
import Control.Monad
import Control.Monad.ST
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.List (nub, mapAccumL)
import Data.STRef
import GHC.Arr (readSTArray, writeSTArray)
import GHC.Utils.Monad
import Data.Foldable ()
import Data.Maybe
import Type
import Data.Bifunctor (first)
import qualified Data.Map.Strict as M
import Data.Map.Strict ( (!?), insert, elems )
import qualified Data.Traversable
import GHC.Records.Compat (HasField(..), getField)
import Control.Lens
import Check.Cxt
import Check.Globals
import Check.Unify
import Check.Infer (infer)

mkLens :: forall x r a . HasField x r a => Lens' r a
mkLens f r = wrap <$> f v
    where (wrap, v) = hasField @x r



lookupGlobalTm :: Name -> Cxt s -> Maybe (ST s Ty)
lookupGlobalTm nm cxt = fmap (gen (cxt^.sUnify) . snd) (view funs $ view globals cxt) !? nm <|> fmap pure ((view conss . view globals) cxt !? nm)

lookupGlobalTy :: Name -> Globals -> Maybe Ty
lookupGlobalTy nm g = view __types g !? nm

declareToplevelFuns :: Cxt s -> [Toplevel] -> Result s (Cxt s)
declareToplevelFuns cxt (TopFun (Fun nm args body ) : defs) = do
  ty <- lift $ freshMeta $ cxt^.sUnify
  case lookupGlobalTm nm cxt of
    Just _ -> except $ Left $ "duplicate definition of " ++ nm
    Nothing -> let vId = length (view vertices $ view globals cxt) in 
      let cxt' =  over (globals . funs) (M.insert nm (RLam args body, ty)) cxt
        in declareToplevelFuns (over (globals . vertices) ((nm, vId) :) cxt') defs
declareToplevelFuns cxt (_ : defs) = declareToplevelFuns cxt defs
declareToplevelFuns cxt [] = pure cxt

{-

declareToplevelData ::  Cxt s -> [Toplevel] -> Result s (Cxt s)
declareToplevelData cxt (TopData (Data nm args conss)  : defs) = do
  (map, ty) <- case args of
            [] -> pure ([], TyData nm [])
            args -> do
              (bounds, tyvars) <- lift $ mapAndUnzipM (\_ -> do TyMeta m <- freshMeta (cxt^.sUnify); pure (m, TyVar m)) args
              pure ([], TyPoly bounds $ TyData nm tyvars)
  cxt <- case lookupGlobalTy nm (view globals cxt) of
            (Just _) -> except $ Left $ "multiple declarations of " ++ nm
            Nothing -> declareToplevelData (over (globals . types) (insert nm ty) cxt) defs
  cxt <- foldlM (\cxt cons -> inferAndBindCons cxt map ty cons) cxt conss
  declareToplevelData cxt defs
declareToplevelData cxt (_ : defs) = declareToplevelData cxt defs
declareToplevelData cxt [] = pure cxt
-}

typecheckToplevelFuns :: Cxt s -> Result s (Cxt s)
typecheckToplevelFuns cxt = do
  (cxt, funs') <- mapAccumLM (uncurry . checkFun) cxt (elems $ view (globals . funs) cxt)
  zipWithM_ (\a b -> unify (cxt^.sUnify) (snd a) (snd b)) funs' (elems $ view (globals . funs) cxt)
  funs' <- lift $ Data.Traversable.mapM (\(l, r) -> (l, ) <$> gen (cxt^.sUnify) r) (_funs $ (view  globals) cxt)
  pure $ set (globals . funs) funs' cxt
    where
      checkFun :: Cxt s -> Raw -> Ty -> Result s (Cxt s, (Raw, Ty))
      checkFun cxt raw ty = do
        (raw, ty') <- infer (over sUnify enterLet cxt) raw
        ty <- unify (cxt^.sUnify) ty ty'
        pure (cxt, (raw, ty))
{-
inferAndBindCons :: Cxt s -> [(Name, MetaVar)] -> Ty -> Ty -> Result s (Cxt s)
inferAndBindCons cxt typevars ty cons'@(RCons nm _) =
  (\ty -> over (globals . conss) (M.insert nm ty) cxt)
    <$> inferCons cxt typevars ty cons'
inferAndBindCons _ _ _ _ = undefined

inferCons :: Cxt s -> [(Name, MetaVar)] -> Ty -> Ty -> Result s Ty
inferCons cxt typevars ty (RCons _ args) = do
  args <- mapM (inferCons cxt typevars ty) args
  pure $ case (args, ty) of
    ([], ty) -> ty
    (args, TyPoly bds ty) -> TyPoly bds (TyArrow args ty)
    (args, ty) -> TyArrow args ty

inferCons cxt typevars ty (ConsApp nm args) = do
  args <- mapM (inferCons cxt typevars ty) args
  pure $ TyData nm args
inferCons _ typevars _ (ConsVar nm) = do
  case lookup nm typevars of
    Just m -> pure $ TyVar m
    Nothing -> except $ Left $ "unbound typevar " ++ nm
-}
{-
  let tyData = case lookupGlobalTy d.name cxt.globals of
                (Just ty) -> ty
                Nothing -> error "impossible"
  case tyData of
    TyPoly args ty -> undefined
    ty -> undefined
  undefined
  where
    inferCon :: Cxt s -> [(Name, Ty)] -> RCons -> Result s Ty
    inferCon cxt vars (RCons nm [args]) = undefined
    inferCon cxt vars (ConsApp nm [args]) = do
      case lookupGlobalCons cxt nm of
        Just 
    inferCon cxt vars (ConsVar nm) = case lookup nm vars of 
      Just ty -> pure ty
      Nothing -> except $ Left $ "unbound typevar " ++ nm 
-}







