{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Check.Infer(infer) where
import Check.Cxt
import Raw.Syntax
import Type
import Common (Constant (..), Name)
import Control.Monad.Trans.Except (ExceptT, except)
import Control.Monad.ST (ST)
import Check.Unify (freshMeta, unify, SUnify, enterLet, inst)
import Control.Monad
import Control.Monad.Trans.Class (lift)
import GHC.Data.IOEnv
import Control.Lens
import Control.Applicative
import Data.List (mapAccumL)
import Control.Arrow (first)

type Result s = ExceptT String (ST s)

bind :: Cxt s -> Name  -> Ty -> Cxt s
bind cxt nm ty = over env ((nm, ty) :) cxt



lookupType :: Cxt s -> Name -> Result s Ty
lookupType _ "true" = pure TBool
lookupType _ "false" = pure TBool
lookupType _ "neg" = pure $ TyArrow [TBool] TBool
lookupType _ "and" = pure $ TyArrow [TBool, TBool] TBool
lookupType _ "or" = pure $ TyArrow [TBool, TBool] TBool
lookupType _ "xor" = pure $ TyArrow [TBool, TBool] TBool
lookupType cxt "eq" = do
  a <- lift $ freshMeta (cxt^.sUnify)
  pure $ TyArrow [a, a] TBool
lookupType cxt "if" = do
  a <- lift $ freshMeta (cxt^.sUnify)
  pure $ TyArrow [TBool, a, a] a
lookupType cxt "nil" = do
  a <- lift $ freshMeta (cxt^.sUnify)
  pure $ TList a
lookupType cxt "cons" = do
  a <- lift $ freshMeta (cxt^.sUnify)
  pure $ TyArrow [a, TList a] (TList a)
lookupType cxt "head" = do
  a <- lift $ freshMeta (cxt^.sUnify)
  pure $ TyArrow [TList a] a
lookupType cxt "tail" = do
  a <- lift $ freshMeta (cxt^.sUnify)
  pure $ TyArrow [TList a] (TList a)
lookupType cxt "snd" = do
  a <- lift $ freshMeta (cxt^.sUnify)
  b <- lift $ freshMeta (cxt^.sUnify)
  pure $ TyArrow [TPair a b] b
lookupType cxt "fst" = do
  a <- lift $ freshMeta (cxt^.sUnify)
  b <- lift $ freshMeta (cxt^.sUnify)
  pure $ TyArrow [TPair a b] a
lookupType cxt "empty" = do
  a <- lift $ freshMeta (cxt^.sUnify)
  pure $ TyArrow [TList a] TBool
lookupType _ "-" = pure $ TyArrow [TInt, TInt] TInt
lookupType _ nm = except $ Left $ "unknown identifier " ++ nm

{-   
infer :: Cxt s -> Raw -> Result s (Raw, Ty)
infer cxt (RApp t args) = do
  (tmArgs, tyArgs) <- mapAndUnzipM (\raw -> snd <$> infer cxt raw) args
  tyResult <- lift $ freshMeta cxt.sUnify
  (_, (tmArrow, tyArrow)) <- infer cxt t
  _ <- unify cxt.sUnify tyArrow (TyArrow tyArgs tyResult)
  pure (cxt, (RApp tmArrow tmArgs, tyResult))
infer cxt (RLam [] tms) = do
  tms <- inferBlock cxt tms
  let ty = snd $ head tms
  pure (cxt, (RBlockTyped tms, ty))
infer cxt (RLam nms tms) = do
  (cxt, tyArgs) <- lift $ mapAccumLM (\cxt nm -> (\ty -> (bind cxt nm ty, ty)) <$> freshMeta cxt._sUnify) cxt nms
  (tm, ty) <- inferBlock cxt tms
  pure (cxt, (RLam nms tm, TyArrow tyArgs ty))
infer cxt (RVar name) = do
  let ty = fmap pure (lookup name (reverse (cxt^.env))) <|> lookupGlobalTm name cxt
  ty <- case ty of
    Just ty -> lift ty
    Nothing -> lookupType cxt name
  case ty of
    (TyPoly bds ty) -> lift $ (RVar name, ) <$> inst (cxt^.sUnify) bds ty
    ty -> pure (RVar name, ty)
infer cxt (RLetRec defs scope) = do
  (cxt, tys) <- lift $ mapAccumLM (\cxt (name, _) -> do m <- freshMeta cxt; cxt <- bind cxt name <$> gen cxt m; pure (cxt, m)) cxt defs
  (tms, tys') <-  mapAndUnzipM (infer (enterLet (cxt^.sUnify)) . snd) defs
  tys <- zipWithM (\ty ty' -> unify cxt ty ty' >>= \ty -> lift $ gen cxt ty) tys tys'
  (cxt, defs) <- pure $ mapAccumL (\cxt ((nm, _), def, ty) -> (bind cxt nm ty, (nm, def, ty))) cxt (zip3 defs tms tys)
  first (RLetRecTyped defs) <$> infer cxt scope
infer cxt (RLet defs scope) = do
  (cxt, defs) <- mapAccumLM (\cxt (nm, def) -> do
    (def, ty) <- infer (enterLet cxt) def
    pure (bind cxt nm ty, (nm, def, ty))) cxt defs
  first (RLetTyped defs) <$> infer cxt scope
infer _ (RConst lit) = pure (RConst lit, inferLit lit)
infer _ _ = undefined
-}

infer :: Cxt s -> Raw -> Result s (Raw, Ty)
infer cxt raw = undefined

{-
infer :: Cxt s -> Raw -> Result s (Raw, Ty)
infer cxt (RApp t args) = do
  (tmArgs, tyArgs) <- mapAndUnzipM (\raw -> infer cxt raw) args
  tyResult <- lift $ freshMeta cxt.sUnify
  (tmArrow, tyArrow) <- infer cxt t
  _ <- unify cxt.sUnify tyArrow (TyArrow tyArgs tyResult)
  pure (RApp tmArrow tmArgs, tyResult)
infer cxt (RLam [] tms) = do
  tms <- inferBlock cxt tms
  let ty = snd $ head tms
  pure (cxt, (RBlockTyped tms, ty))
infer cxt (RLam nms tm) = do
  (cxt, tyArgs) <- lift $ mapAccumLM (\cxt nm -> (\ty -> (bind cxt nm ty, ty)) <$> freshMeta cxt._sUnify) cxt nms
  (tm, ty) <- infer cxt tm
  pure (RLam nms tm, TyArrow tyArgs ty)
infer cxt (RVar name) = do
  let ty = fmap pure (lookup name (reverse (cxt^.env))) <|> lookupGlobalTm name cxt
  ty <- case ty of
    Just ty -> lift ty
    Nothing -> lookupType cxt name
  case ty of
    (TyPoly bds ty) -> lift $ (RVar name, ) <$> inst (cxt^.sUnify) bds ty
    ty -> pure (RVar name, ty)
infer cxt (RLetRec defs scope) = do
  (cxt, tys) <- lift $ mapAccumLM (\cxt (name, _) -> do m <- freshMeta cxt; cxt <- bind cxt name <$> gen cxt m; pure (cxt, m)) (cxt^.sUnify) defs
  (tms, tys') <-  mapAndUnzipM (infer (enterLet (cxt^.sUnify)) . snd) defs
  tys <- zipWithM (\ty ty' -> unify cxt ty ty' >>= \ty -> lift $ gen cxt ty) tys tys'
  (cxt, defs) <- pure $ mapAccumL (\cxt ((nm, _), def, ty) -> (bind cxt nm ty, (nm, def, ty))) cxt (zip3 defs tms tys)
  first (RLetRecTyped defs) <$> infer cxt scope
infer cxt (RLet defs scope) = do
  (cxt, defs) <- mapAccumLM (\cxt (nm, def) -> do
    (def, ty) <- infer (enterLet cxt) def
    pure (bind cxt nm ty, (nm, def, ty))) cxt defs
  first (RLetTyped defs) <$> infer cxt scope
infer _ (RConst lit) = pure (RConst lit, inferLit lit)
infer _ _ = undefined

inferBlock :: Cxt s -> [Raw] -> Result s [(Raw, Ty)]
inferBlock cxt raws = snd <$> mapAccumLM (\cxt raw -> infer cxt raw) cxt raws

-}

inferLit :: Constant -> Ty
inferLit lit = case lit of
  CString _ -> TyBuiltin BString
  CFloat _ -> TyBuiltin BFloat
  CInt _ -> TyBuiltin BInt
  CBool _ -> TyBuiltin BBool
  _ -> undefined

