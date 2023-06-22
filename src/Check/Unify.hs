{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE TemplateHaskell #-}
module Check.Unify where
import Type
import Control.Monad.ST (ST)
import Control.Monad.Trans.Except (ExceptT, except)
import GHC.Arr (STArray, writeSTArray, readSTArray)
import Data.STRef (STRef, readSTRef, writeSTRef)
import Control.Monad
import Control.Monad.Trans.Class
import Data.List (nub)
import GHC.Utils.Monad (mapAccumLM)
import Raw.Syntax (Raw (..))
import Data.Maybe (fromMaybe)
import Control.Lens

type MetaVar = Int

type Result s = ExceptT String (ST s)

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

  abs _ = undefined

  fromInteger = LLvl . fromIntegral

  negate _ = undefined

data MetaEntry = Unsolved LetLvl | Solved Ty deriving (Show)


data SUnify s = SUnify {
    _llvl :: LetLvl,
    _metas :: STArray s Int (Maybe MetaEntry),
    _nextMeta :: STRef s Int
}

$(makeLenses ''SUnify)

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

data LLvls = LLvls {
    new :: LetLvl,
    old :: LetLvl
  }

enterLet :: SUnify s -> SUnify s
enterLet = over llvl (+ 1)


writeMeta :: SUnify s -> MetaVar -> MetaEntry -> ST s ()
--writeMeta cxt m e | trace ("writeMeta: " ++ show m ++ " " ++ show e) False = undefined
writeMeta _ m (Solved (TyMeta m')) | m == m' = error "can't solve meta with itself"
writeMeta cxt m e = writeSTArray (_metas cxt) m (Just e)


inst :: SUnify s -> [MetaVar] -> Ty -> ST s Ty
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
    inst' _ ty = ty

gen :: SUnify s -> Ty -> ST s Ty
--gen cxt ty | trace ("gen" ++ show ty) False = undefined
gen cxt ty = do
  ty <- force cxt ty
  (bds, ty) <- gen' cxt ty
  case bds of
    [] -> pure ty
    bds -> pure $ TyPoly bds ty
   where
    gen' :: SUnify s -> Ty -> ST s ([MetaVar], Ty)
    gen' cxt (TyMeta m) = do
      readMeta cxt m >>= \case
        Unsolved llvl' | llvl' > (cxt ^. llvl) -> pure ([m], TyVar m)
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
    gen' _ ty = pure ([], ty)


-- wei have to make sure when trying to solve a meta originating from a type of or a type inside a let bound at a certain de-bruijn level with types of stuff originating from stuff later bound. solve let bounds by unification with types bound before. metas originating from types of bound lambda values (not of types originating from types of or inside let bounds) with lower de-bruijn levels can be solved by unification with any meta or type bound later but we can't solve metas stemming from let bounds with types stemming from later bound stuff to ensure let polymorphism. after all unifications the unsolved metas should be usable as type variables? all non type variables are solved (the solution can contain other metas (type variables after unification).

-- ALSO DON'T SOLVE METAS ORIGINATING FROM STUFF AT A CERTAIN DE-BRUIJN LEVEL WITH TYPE VARIABLES (UNSOLVED METAS) (SOMEHOW STORE PREVIOUS META. LET BOUNDS CAN HAVE SOLVED METAS) STEMMING ORIGINATING FROM LET BOUNDS WITH LOWER DE-BRUIJN LEVEL. type variables are metas that are TMETA n that force to "TMETA m"s after type checking. tmeta are the same when they force to the same solution

-- wei have to make sure wei don't solve metas originating from let bounds with unification results from types of values bound? at deeper levels. so wei have to pass at what level the type is wei're trying to unify and what's the de bruijn levels the original meta solution is from?
occurs :: SUnify s -> MetaVar -> Ty -> Result s ()
occurs cxt m ty = void $ occurs' cxt m ty
  where
    occurs' :: SUnify s -> MetaVar -> Ty -> Result s Ty
    occurs' _ m (TyMeta m') | m == m' = except $ Left "occurs failed"
    occurs' cxt m (TyMeta m') = do
      lift (readMeta cxt m') >>= \case
        Solved ty -> (occurs' cxt m ty >>= \ty -> lift $ writeMeta cxt m' $ Solved ty) >> pure ty
        Unsolved _ -> pure $ TyMeta m'
    occurs' cxt m (TPair a b) = TPair <$> occurs' cxt m a <*> occurs' cxt m b
    occurs' cxt m (TList a) = TList <$> occurs' cxt m a
    occurs' cxt m (TMaybe a) = TMaybe <$> occurs' cxt m a
    occurs' cxt m (TyArrow a b) = TyArrow <$> mapM (occurs' cxt m) a <*> occurs' cxt m b
    occurs' cxt m (TyData nm tys) = TyData nm <$> mapM (occurs' cxt m) tys
    occurs' _ _ ty = pure ty


freshMeta :: SUnify s -> ST s Ty
freshMeta cxt = do
  m <- readSTRef $ cxt^.nextMeta
  writeSTRef (cxt^.nextMeta) (m + 1)
  writeSTArray (cxt^.metas) m $ Just $ Unsolved (cxt^.llvl)
  pure $ TyMeta m

-- type Result s a = ExceptT String (ST (MCxt s))

readMeta :: SUnify s -> MetaVar -> ST s MetaEntry
readMeta cxt m = do
  m <- readSTArray (cxt^.metas) m
  case m of
    Just m -> pure m
    Nothing -> error "impossible"


data BD = Bound | Defined
  deriving (Show)

force :: SUnify s -> Ty -> ST s Ty
force cxt (TyMeta m') = do
  readMeta cxt m' >>= \case
    Solved ty -> (force cxt ty >>= \ty -> writeMeta cxt m' (Solved ty) >> pure ty)
    Unsolved _ -> pure $ TyMeta m'
force cxt (TPair a b) = TPair <$> force cxt a <*> force cxt b
force cxt (TList a) = TList <$> force cxt a
force cxt (TMaybe a) = TMaybe <$> force cxt a
force cxt (TyArrow a b) = TyArrow <$> mapM (force cxt) a <*> force cxt b
force cxt (TyData nm tys) = TyData nm <$> mapM (force cxt) tys
force _ ty = pure ty

forceTm :: SUnify s -> Raw -> ST s Raw
forceTm cxt (RLetTyped defs tm) = RLetTyped <$> mapM (\(nm, def, ty) -> (nm, , ) <$> forceTm cxt def <*> force cxt ty) defs <*> forceTm cxt tm
forceTm cxt (RLetRecTyped defs tm) = RLetRecTyped <$> mapM (\(nm, def, ty) -> (nm, , ) <$> forceTm cxt def <*> force cxt ty) defs <*> forceTm cxt tm
forceTm cxt (RLam nms tm) = RLam nms <$> forceTm cxt tm
forceTm cxt (RApp tm args) = RApp <$> forceTm cxt tm <*> mapM (forceTm cxt) args
forceTm _ tm = pure tm



unify :: SUnify s -> Ty -> Ty -> Result s Ty
unify cxt l r = case l == r of
  True -> pure l
  _ -> do
    l <- lift $ force cxt l
    r <- lift $ force cxt r
    unify' cxt l r
  where
    unify' :: SUnify s -> Ty -> Ty -> Result s Ty
    unify' cxt l r = case (l, r) of
      (TyMeta m         , TyMeta m'      ) | m == m' -> pure $ TyMeta m
      (TyMeta m         , TyMeta m'      )
        | (m, m') <- (min m m', max m m' ) -> lift (writeMeta cxt m' $ Solved (TyMeta m)) >> pure (TyMeta m)
      (TyMeta m         , ty             ) -> do occurs cxt m ty; lift $ writeMeta cxt m $ Solved ty; pure ty
      (ty               , TyMeta m       ) -> do occurs cxt m ty; lift $ writeMeta cxt m $ Solved ty; pure ty
      (TyArrow [a] b    , TyArrow [a'] b') -> TyArrow <$> fmap pure (unify cxt a a') <*> unify cxt b b'
      (TyArrow (a: as) b    , TyArrow [a'] b') -> TyArrow <$> fmap pure (unify cxt a a') <*> unify cxt (TyArrow as b) b'
      (TyArrow [a] b    , TyArrow (a' : as') b') -> TyArrow <$> fmap pure (unify cxt a a') <*> unify cxt b (TyArrow as' b')
      (TyArrow (a: as) b    , TyArrow (a' : as') b') -> TyArrow <$> fmap pure (unify cxt a a') <*> unify cxt (TyArrow as b) (TyArrow as' b')
      --(TyArrow a b      , TyArrow a' b'  ) -> TyArrow <$> zipWithM (unify cxt) a a' <*> unify cxt b b'
      (TPair a b        , TPair a' b'    ) -> TPair <$> unify cxt a a' <*> unify cxt b b'
      (TList a          , TList a'       ) -> TList <$> unify cxt a a'
      (TMaybe a         , TMaybe a'      ) -> TMaybe <$> unify cxt a a'
      (TyPoly bds ty    , ty'            ) -> do
        ty <- lift $ inst (enterLet cxt) bds ty
        ty <- unify cxt ty ty'
        lift $ gen cxt ty
      (ty               , TyPoly bds ty') -> do
        ty' <- lift $ inst (enterLet cxt) bds ty'
        ty <- unify cxt ty ty'
        lift $ gen cxt ty
      (l                , r             ) -> case l == r of
        True -> pure l
        False -> except $ Left $ "failed to unify " ++ show l ++ " with " ++ show r

