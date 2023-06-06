
module Check where

import Raw.Syntax

import Common (Name, Constant (..))
import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Data.IORef
import Data.List (elemIndex, find, findIndex, nub, mapAccumL)
import Data.STRef
import GHC.Arr (STArray, newSTArray, readSTArray, writeSTArray)
import GHC.IO (unsafeDupablePerformIO)
import GHC.Utils.Monad
import Control.Monad.Zip
import Data.Foldable
import Debug.Trace (trace)
import Control.Exception
import qualified Data.Maybe
import Data.Maybe
import Type
import GHC.Plugins (mapAndUnzip)
import qualified Data.Bifunctor
import Data.Bifunctor (first)
import qualified Data.Map.Strict as M
import Data.Map.Strict ((!?), insert)
import Data.Semigroup.Foldable (foldlM1)

-- we want don't want to unify the types inside let bounds with types of bounds with deeper level
data LetLvl = Marked | W | LLvl Int deriving (Show, Eq)

instance Num LetLvl where
  (+) W _ = W
  (+) _ W = W
  (+) (LLvl a) (LLvl b) = LLvl $ a + b
  (+) _ _ = undefined

  (-) _ W = error "not defined"
  (-) W _ = W
  (-) (LLvl a) (LLvl b) = LLvl $ a - b
  (-) _ _ = undefined

  (*) _ _ = undefined

  signum _ = undefined

  abs a = undefined

  fromInteger = LLvl . fromIntegral

  negate _ = undefined



instance Ord LetLvl where
  compare W W = EQ
  compare W (LLvl _) = GT
  compare (LLvl _) W = LT
  compare (LLvl a) (LLvl b) = compare a b
  compare _ _ = undefined

  (<=) _ W = True
  (<=) W (LLvl _) = False
  (<=) (LLvl a) (LLvl b) = a <= b
  (<=) _ _ = undefined

-- data MetaEntry = Entry { llvl :: LetLvl, bd :: BD, solution :: Maybe Ty }
data MetaEntry = Unsolved LetLvl | Solved Ty deriving (Show)

data LLvls = LLvls {
    new :: LetLvl,
    old :: LetLvl
  }


freshMeta :: Cxt s -> ST s Ty
freshMeta cxt = do
  m <- readSTRef $ cxt.nextMeta
  writeSTRef cxt.nextMeta (m + 1)
  writeSTArray cxt.metas m $ Just $ Unsolved cxt.llvl
  pure $ TyMeta m

-- type Result s a = ExceptT String (ST (MCxt s))

readMeta :: Cxt s -> MetaVar -> ST s MetaEntry
readMeta cxt m = do
  m <- readSTArray cxt.metas m
  case m of
    Just m -> pure m
    Nothing -> error "impossible"


data BD = Bound | Defined
  deriving (Show)




pattern TString :: Ty
pattern TString = TyBuiltin PString
pattern TFloat = TyBuiltin PFloat
pattern TVoid = TyBuiltin PVoid
pattern TPair a b = TyBuiltin (PPair a b)
pattern TList a = TyBuiltin (PList a)
pattern TMaybe a = TyBuiltin (PMaybe a)
pattern TBool = TyBuiltin PBool
pattern TInt = TyBuiltin PInt

--runExceptT $ pure $ runST $ pure (TyMeta 0)



type Lvl = Int

data Globals = Globals {
  funs :: M.Map String (Raw, Ty),
  cons :: M.Map String Ty,
  types :: M.Map String Ty
}

data Cxt s = Cxt {
  metas :: STArray s Int (Maybe MetaEntry),
  nextMeta :: STRef s Int,
  env :: [(Name, Ty)],
  llvl :: LetLvl,
  globals :: Globals
}


lookupGlobalTm :: Name -> Globals -> Maybe Ty
lookupGlobalTm nm g = fmap snd g.funs !? nm <|> g.cons !? nm

lookupGlobalTy :: Name -> Globals -> Maybe Ty
lookupGlobalTy nm g = g.types !? nm

lookupGlobalCons :: Name -> Globals -> Maybe Ty
lookupGlobalCons nm g = g.cons !? nm

declareToplevelFuns :: Cxt s -> [Toplevel] -> Result s (Cxt s)
declareToplevelFuns cxt (TopFun f : defs) = do
  let nm = f.name
  args <- lift $ mapM (\_ -> freshMeta cxt) f.args
  ret <- lift $ freshMeta cxt
  case lookupGlobalTm nm cxt.globals of
    Just ty -> except $ Left $ "duplicate definition of " ++ nm
    Nothing -> pure $ cxt { globals = cxt.globals { funs = M.insert nm (RLam f.args f.body, TyArrow args ret) cxt.globals.funs } }
declareToplevelFuns cxt (_ : defs) = declareToplevelFuns cxt defs
declareToplevelFuns cxt [] = pure cxt

declareToplevelData ::  Cxt s -> [Toplevel] -> Result s (Cxt s)
declareToplevelData cxt (TopData d : defs) = do
  let nm = d.name
  (map, ty) <- case d.args of
            [] -> pure ([], TyData nm [])
            args -> do
              (map, bounds, tyvars) <- lift $ mapAndUnzip3M (\nm -> do TyMeta m <- freshMeta cxt; pure ((nm, m), m, TyVar m)) d.args
              pure ([], TyPoly bounds $ TyData nm tyvars)
  cxt <- case lookupGlobalTy nm cxt.globals of
            (Just _) -> except $ Left $ "multiple declarations of " ++ nm
            Nothing -> declareToplevelData (cxt{ globals = cxt.globals{ types = insert nm ty cxt.globals.types } }) defs
  cxt <- foldlM (\cxt cons -> inferAndBindCons cxt map ty cons) cxt d.cons
  declareToplevelData cxt defs
declareToplevelData cxt (_ : defs) = declareToplevelData cxt defs
declareToplevelData cxt [] = pure cxt

typecheckToplevelData :: Cxt s -> Result s ()
typecheckToplevelData cxt = undefined
{-
typecheckToplevelFuns :: Cxt s -> Result s (Cxt s)
typecheckToplevelFuns cxt = do
  cxt <- M.mapAccumWithKey <$> (\cxt nm (raw, ty) -> (checkFun cxt)) cxt cxt.globals.funs
  pure $ cxt { globals = cxt.globals { funs = funs } }
    where
      checkFun :: Cxt s -> Name -> Raw -> Ty -> Result s (Cxt s, (Raw, Ty))
      checkFun cxt raw ty = do
        let cxt = enterLet cxt
        (raw, ty) <- infer cxt raw
        let cxt = leaveLet cxt
        (raw, ) <$> lift $ gen cxt ty
-}

instance MonadFail (Result s) where
  fail = pure $ except $ Left "impossible"


inferAndBindCons :: Cxt s -> [(Name, MetaVar)] -> Ty -> Cons -> Result s (Cxt s)
inferAndBindCons cxt typevars ty cons@(Cons nm args) = (\ty -> cxt { globals = cxt.globals { cons = M.insert nm ty cxt.globals.cons } }) <$> inferCons cxt typevars ty cons

inferCons :: Cxt s -> [(Name, MetaVar)] -> Ty -> Cons -> Result s Ty
inferCons cxt typevars ty (Cons nm args) = do
  args <- mapM (inferCons cxt typevars ty) args
  pure $ case ty of
              TyPoly bds ty -> TyPoly bds (TyArrow args ty)
              ty -> TyArrow args ty

inferCons cxt typevars ty (ConsApp nm args) = do
  args <- mapM (inferCons cxt typevars ty) args
  pure $ TyData nm args
inferCons cxt typevars ty (ConsVar nm) = do
  case lookup nm typevars of
    Just m -> pure $ TyVar m
    Nothing -> except $ Left $ "unbound typevar " ++ nm

{-
  let tyData = case lookupGlobalTy d.name cxt.globals of
                (Just ty) -> ty
                Nothing -> error "impossible"
  case tyData of
    TyPoly args ty -> undefined
    ty -> undefined
  undefined
  where
    inferCon :: Cxt s -> [(Name, Ty)] -> Cons -> Result s Ty
    inferCon cxt vars (Cons nm [args]) = undefined
    inferCon cxt vars (ConsApp nm [args]) = do
      case lookupGlobalCons cxt nm of
        Just 
    inferCon cxt vars (ConsVar nm) = case lookup nm vars of 
      Just ty -> pure ty
      Nothing -> except $ Left $ "unbound typevar " ++ nm 
-}

bind :: Cxt s -> Name  -> Ty -> Cxt s
bind cxt nm ty = cxt { env = (nm, ty) : cxt.env }

enterLet :: Cxt s -> Cxt s
enterLet cxt = cxt { llvl = 1 + cxt.llvl }

leaveLet :: Cxt s -> Cxt s
leaveLet cxt = cxt { llvl = cxt.llvl - 1 }


lookupType :: Cxt s -> Name -> Result s Ty
lookupType cxt@Cxt { env = (nm, ty) : env } nm'
  = if nm == nm'
      then pure ty
      else lookupType cxt { env } nm'
lookupType cxt "true" = pure TBool
lookupType cxt "false" = pure TBool
lookupType cxt "neg" = pure $ TyArrow [TBool] TBool
lookupType cxt "and" = pure $ TyArrow [TBool, TBool] TBool
lookupType cxt "or" = pure $ TyArrow [TBool, TBool] TBool
lookupType cxt "xor" = pure $ TyArrow [TBool, TBool] TBool
lookupType cxt "eq" = do
  a <- lift $ freshMeta cxt
  pure $ TyArrow [a, a] TBool
lookupType cxt "if" = do
  a <- lift $ freshMeta cxt
  pure $ TyArrow [TBool, a, a] a
lookupType cxt "nil" = do
  a <- lift $ freshMeta cxt
  pure $ TList a
lookupType cxt "cons" = do
  a <- lift $ freshMeta cxt
  pure $ TyArrow [a, TList a] (TList a)
lookupType cxt "head" = do
  a <- lift $ freshMeta cxt
  pure $ TyArrow [TList a] a
lookupType cxt "tail" = do
  a <- lift $ freshMeta cxt
  pure $ TyArrow [TList a] (TList a)
lookupType cxt "snd" = do
  a <- lift $ freshMeta cxt
  b <- lift $ freshMeta cxt
  pure $ TyArrow [TPair a b] b
lookupType cxt "fst" = do
  a <- lift $ freshMeta cxt
  b <- lift $ freshMeta cxt
  pure $ TyArrow [TPair a b] a
lookupType cxt "empty" = do
  a <- lift $ freshMeta cxt
  pure $ TyArrow [TList a] TBool
lookupType cxt "-" = pure $ TyArrow [TInt, TInt] TInt
lookupType cxt nm = except $ Left $ "unknown identifier " ++ nm


inst :: Cxt s -> [MetaVar] -> Ty -> ST s Ty
inst cxt bounds ty = do
  subst <- mapM (\x -> do y <- freshMeta cxt; pure (x, y)) bounds
  pure $ inst' subst ty
  where
    inst' :: [(MetaVar, Ty)] -> Ty -> Ty
    inst' subs (TyVar a) = fromMaybe (TyVar a) $ lookup a subs
    inst' subs (TyArrow a b) = TyArrow (fmap (inst' subs) a) (inst' subs b)
    inst' subs (TyData name tys) = TyData name $ map (inst' subs) tys
    inst' subs (TMaybe ty) = TMaybe $ inst' subs ty
    inst' subs (TList ty) = TList $ inst' subs ty
    inst' subs (TPair a b) = TPair (inst' subs a) (inst' subs b)
    inst' subs ty = ty

gen :: Cxt s -> Ty -> ST s Ty
--gen cxt ty | trace ("gen" ++ show ty) False = undefined
gen cxt ty = do
  ty <- force cxt ty
  (bds, ty) <- gen' cxt ty
  case bds of
    [] -> pure ty
    bds -> pure $ TyPoly bds ty
   where
    gen' :: Cxt s -> Ty -> ST s ([MetaVar], Ty)
    gen' cxt@Cxt { llvl } (TyMeta m) = do
      readMeta cxt m >>= \case
        Unsolved llvl' | llvl' > llvl -> pure ([m], TyVar m)
        Solved ty -> gen' cxt ty
        _ -> pure ([], TyMeta m)
    gen' cxt (TyArrow a b) = do
      (bds , a) <- mapAccumLM (\bds -> fmap (\(bds', ty) -> (nub (bds ++ bds'), ty)) . gen' cxt) [] a
      (bds', b) <- gen' cxt b
      pure (nub (bds ++ bds'), TyArrow a b)
    gen' cxt (TPair a b) = do
      (bds , a) <- gen' cxt a
      (bds', b) <- gen' cxt b
      pure (nub (bds ++ bds'), TPair a b)
    gen' cxt (TList a) = do (vars, a) <- gen' cxt a; pure (vars, TList a)
    gen' cxt (TMaybe a) = do (vars, a) <- gen' cxt a; pure (vars, TMaybe a)
    gen' cxt (TyData nm tys) = do
      (bds, tys) <- mapAccumLM (\bds ty -> do
        (bds', ty) <- gen' cxt ty
        pure (bds ++ bds', ty)) [] tys
      pure (nub bds, TyData nm tys)
    gen' cxt ty = pure ([], ty)


-- wei have to make sure when trying to solve a meta originating from a type of or a type inside a let bound at a certain de-bruijn level with types of stuff originating from stuff later bound. solve let bounds by unification with types bound before. metas originating from types of bound lambda values (not of types originating from types of or inside let bounds) with lower de-bruijn levels can be solved by unification with any meta or type bound later but we can't solve metas stemming from let bounds with types stemming from later bound stuff to ensure let polymorphism. after all unifications the unsolved metas should be usable as type variables? all non type variables are solved (the solution can contain other metas (type variables after unification).

-- ALSO DON'T SOLVE METAS ORIGINATING FROM STUFF AT A CERTAIN DE-BRUIJN LEVEL WITH TYPE VARIABLES (UNSOLVED METAS) (SOMEHOW STORE PREVIOUS META. LET BOUNDS CAN HAVE SOLVED METAS) STEMMING ORIGINATING FROM LET BOUNDS WITH LOWER DE-BRUIJN LEVEL. type variables are metas that are TMETA n that force to "TMETA m"s after type checking. tmeta are the same when they force to the same solution

-- wei have to make sure wei don't solve metas originating from let bounds with unification results from types of values bound? at deeper levels. so wei have to pass at what level the type is wei're trying to unify and what's the de bruijn levels the original meta solution is from?
occurs :: Cxt s -> MetaVar -> Ty -> Result s ()
occurs cxt m ty = void $ occurs' cxt m ty
  where
    occurs' :: Cxt s -> MetaVar -> Ty -> Result s Ty
    occurs' cxt m (TyMeta m') | m == m' = except $ Left "occurs failed"
    occurs' cxt m (TyMeta m') = do
      lift (readMeta cxt m') >>= \case
        Solved ty -> (occurs' cxt m ty >>= \ty -> lift $ writeMeta cxt m' $ Solved ty) >> pure ty
        Unsolved llvl -> pure $ TyMeta m'
    occurs' cxt m (TPair a b) = TPair <$> occurs' cxt m a <*> occurs' cxt m b
    occurs' cxt m (TList a) = TList <$> occurs' cxt m a
    occurs' cxt m (TMaybe a) = TMaybe <$> occurs' cxt m a
    occurs' cxt m (TyArrow a b) = TyArrow <$> mapM (occurs' cxt m) a <*> occurs' cxt m b
    occurs' cxt m (TyData nm tys) = TyData nm <$> mapM (occurs' cxt m) tys
    occurs' cxt m ty = pure ty


force :: Cxt s -> Ty -> ST s Ty
force cxt (TyMeta m') = do
  readMeta cxt m' >>= \case
    Solved ty -> (force cxt ty >>= \ty -> writeMeta cxt m' (Solved ty) >> pure ty)
    Unsolved llvl -> pure $ TyMeta m'
force cxt (TPair a b) = TPair <$> force cxt a <*> force cxt b
force cxt (TList a) = TList <$> force cxt a
force cxt (TMaybe a) = TMaybe <$> force cxt a
force cxt (TyArrow a b) = TyArrow <$> mapM (force cxt) a <*> force cxt b
force cxt (TyData nm tys) = TyData nm <$> mapM (force cxt) tys
force cxt ty = pure ty

forceTm :: Cxt s -> Raw -> ST s Raw
forceTm cxt (RLetTyped defs tm) = RLetTyped <$> mapM (\(nm, def, ty) -> (nm, , ) <$> forceTm cxt def <*> force cxt ty) defs <*> forceTm cxt tm
forceTm cxt (RLetRecTyped defs tm) = RLetRecTyped <$> mapM (\(nm, def, ty) -> (nm, , ) <$> forceTm cxt def <*> force cxt ty) defs <*> forceTm cxt tm
forceTm cxt (RLam nms tm) = RLam nms <$> forceTm cxt tm
forceTm cxt (RApp tm args) = RApp <$> forceTm cxt tm <*> mapM (forceTm cxt) args
forceTm cxt tm = pure tm

writeMeta :: Cxt s -> MetaVar -> MetaEntry -> ST s ()
--writeMeta cxt m e | trace ("writeMeta: " ++ show m ++ " " ++ show e) False = undefined
writeMeta cxt m (Solved (TyMeta m')) | m == m' = error "can't solve meta with itself"
writeMeta cxt m e = writeSTArray cxt.metas m (Just e)

unify :: Cxt s -> Ty -> Ty -> Result s Ty
unify cxt l r = if l == r then pure l else do
  _ <- trace ("unify (" ++ show l ++ ") (" ++ show r ++ ")") (pure ())
  if l == r then
    pure l
  else do
    l <- lift $ force cxt l
    r <- lift $ force cxt r
    unify' cxt l r
  where
  unify' cxt l r = case (l, r) of
    (TyMeta m         , TyMeta m'     ) | m == m' -> pure $ TyMeta m
    (TyMeta m         , TyMeta m'     )
      | (m, m') <- (min m m', max m m') -> lift (writeMeta cxt m' $ Solved (TyMeta m)) >> pure (TyMeta m)
    (TyMeta m         , ty            ) -> do occurs cxt m ty; lift $ writeMeta cxt m $ Solved ty; pure ty
    (ty               , TyMeta m      ) -> do occurs cxt m ty; lift $ writeMeta cxt m $ Solved ty; pure ty
    (TyArrow a b      , TyArrow a' b' ) -> TyArrow <$> zipWithM (unify cxt) a a' <*> unify cxt b b'
    (TPair a b        , TPair a' b'   ) -> TPair <$> unify cxt a a' <*> unify cxt b b'
    (TList a          , TList a'      ) -> TList <$> unify cxt a a'
    (TMaybe a         , TMaybe a'     ) -> TMaybe <$> unify cxt a a'
    (TyPoly bds ty    , ty'           ) -> do
      ty <- lift $ inst (enterLet cxt) bds ty
      ty <- unify cxt ty ty'
      lift $ gen cxt ty
    (ty               , TyPoly bds ty') -> do
      ty' <- lift $ inst (enterLet cxt) bds ty'
      ty <- unify cxt ty ty'
      lift $ gen cxt ty
    (l                , r             ) -> if l == r
      then pure l
      else except $ Left $ "failed to unify " ++ show l ++ " with " ++ show r


type Result s = ExceptT String (ST s)

inferPattern :: Cxt s -> Pattern -> Raw -> ST s (Ty, Ty)
inferPattern cxt (PCons nm args) = do
  undefined

lookupCons :: Cxt s -> Name -> Result s ()
lookupCons = undefined

infer :: Cxt s -> Raw -> Result s (Raw, Ty)
-- infer cxt raw | trace ("infer " ++ show raw) False = undefined
infer cxt (RApp t args) = do
  (tmArgs, tyArgs) <- mapAndUnzipM (infer cxt) args
  tyResult <- lift $ freshMeta cxt
  (tmArrow, tyArrow) <- infer cxt t
  unify cxt tyArrow (TyArrow tyArgs tyResult)
  pure (RApp tmArrow tmArgs, tyResult)
infer cxt (RLam nms tm) = do
  (cxt, tyArgs) <- lift $ mapAccumLM (\cxt nm -> (\ty -> (bind cxt nm ty, ty)) <$> freshMeta cxt) cxt nms
  (tm, ty) <- infer cxt tm
  pure (RLam nms tm, TyArrow tyArgs ty)
infer cxt (RVar name) = do
  lookupType cxt name >>= \case
    (TyPoly bds ty) -> lift $ (RVar name, ) <$> inst cxt bds ty
    ty -> pure (RVar name, ty)
infer cxt (RLetRec defs scope) = do
  (cxt, tys) <- lift $ mapAccumLM (\cxt (name, def) -> do m <- freshMeta cxt; cxt <- pure $ bind cxt name m; pure (cxt, m)) cxt defs
  cxt <- return $ enterLet cxt
  (tms, tys') <- mapAndUnzipM (infer cxt . snd) defs
  cxt <- return $ leaveLet cxt
  tys <- zipWithM (unify cxt) tys tys'
  defs <- lift $ mapM (\((nm, def), ty) -> (nm, def,) <$> gen cxt ty) (zip defs tys)
  cxt <- pure $ foldl (\cxt (nm, _, ty) -> bind cxt nm ty) cxt defs
  first (RLetRecTyped defs) <$> infer cxt scope
infer cxt (RLet defs scope) = do
  (cxt, defs) <- mapAccumLM (\cxt (nm, def) -> do
    cxt <- return $ enterLet cxt
    (def, ty) <- infer cxt def
    cxt <- return $ leaveLet cxt
    pure (bind cxt nm ty, (nm, def, ty))) cxt defs
  first (RLetTyped defs) <$> infer cxt scope
infer cxt (RConst lit) = pure (RConst lit, inferLit lit)


mkCxt :: ST s (Cxt s)
mkCxt = do
  metas <- newSTArray (0, 8192) Nothing
  nextMeta <- newSTRef 0
  pure $ Cxt {
    metas,
    nextMeta,
    llvl = 1,
    env = [],
    globals = Globals { cons = M.empty, funs = M.empty, types = M.empty }
  }


inferLit :: Constant -> Ty
inferLit lit = case lit of
  CString _ -> TyBuiltin PString
  CFloat _ -> TyBuiltin PFloat
  CInt _ -> TyBuiltin PInt
  CBool _ -> TyBuiltin PBool

