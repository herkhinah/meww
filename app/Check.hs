{-# LANGUAGE DisambiguateRecordFields #-}

module Check where

import Common (Name)
import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Data.IORef
import Data.List (elemIndex, find, findIndex, nub)
import Data.Map
import Data.STRef
import GHC.Arr (STArray, newSTArray, readSTArray, writeSTArray)
import GHC.IO (unsafeDupablePerformIO)
import GHC.Utils.Monad
import Syntax
import Control.Monad.Zip
import Data.Foldable
import Debug.Trace (trace)
import Control.Exception

-- we want don't want to unify the types inside let bounds with types of bounds with deeper level
type MetaVar = Int

type TypeVar = Int

data LetLvl = W | LLvl Int deriving (Show, Eq)

instance Num LetLvl where
  (+) W _ = W
  (+) _ W = W
  (+) (LLvl a) (LLvl b) = LLvl $ a + b

  (-) _ W = error "not defined"
  (-) W _ = W
  (-) (LLvl a) (LLvl b) = LLvl $ a - b

  (*) _ _ = undefined

  abs a = a

  fromInteger = LLvl . fromIntegral

  negate _ = undefined



instance Ord LetLvl where
  compare W W = EQ
  compare W (LLvl _) = GT
  compare (LLvl _) W = LT
  compare (LLvl a) (LLvl b) = compare a b

  (<=) _ W = True
  (<=) W (LLvl _) = False
  (<=) (LLvl a) (LLvl b) = a <= b

-- data MetaEntry = Entry { llvl :: LetLvl, bd :: BD, solution :: Maybe Ty }
data MetaEntry = Unsolved LetLvl | Solved Marked Ty deriving (Show)

data Marked = Unmarked | Marker Int deriving (Show)

data MCxt s = MCxt
  { metas :: STArray s Int (Maybe MetaEntry),
    nextMeta :: STRef s Int
  }

newMCxt :: ST s (MCxt s)
newMCxt = do
  metas <- newSTArray (0, 8192) Nothing
  nextMeta <- newSTRef 0
  pure MCxt {metas = metas, nextMeta = nextMeta}

freshMeta :: Cxt s -> ST s Ty
freshMeta cxt = do
  m <- readSTRef $ cxt.mcxt.nextMeta
  writeSTRef (nextMeta cxt.mcxt) (m + 1)
  writeSTArray (metas cxt.mcxt) m $ Just $ Unsolved cxt.llvl
  pure $ TMeta m

-- type Result s a = ExceptT String (ST (MCxt s))

lookupMeta :: Cxt s -> MetaVar -> ST s MetaEntry
lookupMeta cxt m = do
  m <- readSTArray cxt.mcxt.metas m
  case m of
    Just m -> pure m
    Nothing -> error "impossible"

updateLetLvl :: Cxt s -> MetaVar -> LetLvl -> ST s ()
updateLetLvl cxt m llvl = do
  readSTArray cxt.mcxt.metas m >>= \case
    Just (Unsolved llvl') | llvl < llvl' -> writeSTArray cxt.mcxt.metas m (Just $ Unsolved llvl)
    _ -> pure ()

type Ix = Int

data BD = Bound | Defined
  deriving (Show)

topFuns :: IORef (Map String [Ty])
topFuns = undefined

topData :: IORef (Map String [Ty])
topData = undefined

data Ty
  = TData Name [Ty]
  | TVar TypeVar
  | TMeta MetaVar
  | TArrow Ty Ty
  | TBuiltin TBuiltin
  | TPoly [TypeVar] Ty deriving (Eq)

instance Show Ty where
  show :: Ty -> String
  show (TVar v) = "\"" ++ show v
  show (TMeta m) = "'" ++ show m
  show (TArrow a@(TArrow _ _) b) = "(" ++ show a ++ ") -> " ++ show b
  show (TArrow a b) = show a ++ " -> " ++ show b
  show (TBuiltin builtin) = show builtin
  show (TPoly vars ty) = "forall " ++ unwords (fmap show vars) ++ ". " ++ show ty

data TBuiltin = PString | PFloat | PVoid | PPair Ty Ty | PList Ty | PMaybe Ty | PBool deriving (Eq)

instance Show TBuiltin where
  show PString = "String"
  show PFloat = "Float"
  show PVoid = "Void"
  show PBool = "Bool"
  show (PPair a b) = "(" ++ show a ++ ", " ++ show b ++ ")"
  show (PList a) = "[" ++ show a ++ "]"
  show (PMaybe a) = "Maybe (" ++ show a ++ ")"

pattern TString = TBuiltin PString
pattern TFloat = TBuiltin PFloat
pattern TVoid = TBuiltin PVoid
pattern TPair a b = TBuiltin (PPair a b)
pattern TList a = TBuiltin (PList a)
pattern TMaybe a = TBuiltin (PMaybe a)
pattern TBool = TBuiltin PBool

--runExceptT $ pure $ runST $ pure (TMeta 0)

data Tm
  = Var Ix
  | App Tm Tm
  | Lam Tm
  | Let Tm Tm
  | PrimOp PrimOp
  | Lit Literal

type Env = [Val]

type Spine = [Val]

data Closure = Closure Env Tm

type Lvl = Int

data Val
  = VRigid Lvl Spine
  | VLam Closure
  | VLit Literal

eval :: Env -> Tm -> Val
eval env (Var ix) = undefined
eval env (App rator and) = undefined
eval env (Lam body) = undefined
eval env (Let t u) = undefined
eval env (Lit lit) = undefined
eval env (PrimOp op) = VLam $ closePrimOp env op

arity :: PrimOp -> Int
arity Plus = 2
arity Minus = 2
arity Mult = 2
arity Print = 1

lam :: Lvl -> Ix -> Tm -> Tm
lam 0 ix tm = tm
lam lvl ix tm = lam (lvl - 1) (ix + 1) (Lam (App tm (Var ix)))

closePrimOp :: Env -> PrimOp -> Closure
closePrimOp env op = Closure env $ lam (arity op) 0 (PrimOp op)

data Cxt s = Cxt {
  mcxt :: MCxt s,
  env :: [(Name, Ty)],
  llvl :: LetLvl
}

bind :: Cxt s -> Name  -> Ty -> Cxt s
bind Cxt { mcxt, llvl, env } nm ty = Cxt { env = (nm, ty) : env, llvl, mcxt }

enterLet :: Cxt s -> Cxt s
enterLet Cxt { mcxt, llvl, env } = Cxt { env, llvl = llvl + 1, mcxt} 

leaveLet :: Cxt s -> Cxt s
leaveLet Cxt { mcxt, llvl, env } = Cxt { env, llvl = llvl -1, mcxt }

lookupBuiltin :: Cxt s -> Name -> ST s Ty
lookupBuiltin cxt "pair" = do
    a <- freshMeta cxt
    b <- freshMeta cxt
    pure $ TArrow a (TArrow b (TPair a b))
lookupBuiltin cxt "fst" = do
    a <- freshMeta cxt
    b <- freshMeta cxt
    pure $ TArrow (TPair a b) a
lookupBuiltin cxt "snd" = do
    a <- freshMeta cxt
    b <- freshMeta cxt
    pure $ TArrow (TPair a b) b


lookupType :: Cxt s -> Name -> Result s Ty
lookupType Cxt { env = (nm, ty) : rem, mcxt, llvl } nm' 
  = if nm == nm' 
      then pure ty 
      else lookupType Cxt { env = rem, mcxt, llvl } nm'
lookupType cxt "true" = pure TBool
lookupType cxt "false" = pure TBool
lookupType cxt "neg" = pure $ TArrow TBool (TArrow TBool TBool)
lookupType cxt "and" = pure $ TArrow TBool (TArrow TBool TBool)
lookupType cxt "or" = pure $ TArrow TBool (TArrow TBool TBool)
lookupType cxt "xor" = pure $ TArrow TBool (TArrow TBool TBool)
lookupType cxt "if" = do
  a <- lift $ freshMeta cxt
  pure $ TArrow TBool (TArrow a (TArrow a a))
lookupType cxt "nil" = do
  a <- lift $ freshMeta cxt
  pure $ TList a
lookupType cxt "cons" = do
  a <- lift $ freshMeta cxt
  pure $ TArrow a (TArrow (TList a) (TList a))
lookupType cxt "head" = do
  a <- lift $ freshMeta cxt
  pure $ TArrow (TList a) a
lookupType cxt "tail" = do
  a <- lift $ freshMeta cxt
  pure $ TArrow (TList a) (TList a)
lookupType cxt "snd" = do
  a <- lift $ freshMeta cxt
  b <- lift $ freshMeta cxt
  pure $ TArrow (TPair a b) b
lookupType cxt "fst" = do
  a <- lift $ freshMeta cxt
  b <- lift $ freshMeta cxt
  pure $ TArrow (TPair a b) a
lookupType cxt "empty" = do
  a <- lift $ freshMeta cxt
  pure $ TArrow (TList a) TBool
lookupType cxt nm = except $ Left $ "unknown identifier " ++ nm

{-
force :: MCxt s -> Ty -> ST s Ty
force mcxt ty = do
  res <- force' mcxt ty
  trace ("force " ++ show ty ++ ": " ++ show res) (pure ())
  pure res

force' :: MCxt s -> Ty -> ST s Ty
force' mcxt (TData nm tys)  = TData nm <$> (mapM $ force mcxt) tys
force' mcxt (TArrow a b) = TArrow <$> force mcxt a <*> force mcxt b
force' mcxt (TMeta m) = do
  lookupMeta mcxt m >>= (\case
    Unsolved _ -> pure $ TMeta m
    Solved (TMeta m') | m == m' -> error "impossible: meta solved itself?"
    Solved val -> do
      val <- force mcxt val >>= \case
         (TMeta m') | m == m' -> error "impossible: meta solved itself? 2"
         val -> pure val
      writeMeta mcxt m (Solved val)
      pure val)
force' mcxt (TList a) = do
  a <- force mcxt a
  pure $ TList a
force' mcxt (TPair a b) = do
  a <- force mcxt a
  b <- force mcxt b
  pure $ TPair a b
force' mcxt ty = pure ty
-}
infixl 8 $$

($$) :: Closure -> Val -> Val
($$) (Closure env t) u = eval (u : env) t

gen :: Cxt s -> Ty -> ST s Ty
gen cxt ty | trace ("gen" ++ show ty) False = undefined
gen cxt ty = do
  (bds, ty) <- gen' cxt ty
  pure $ TPoly bds ty
   where
    gen' :: Cxt s -> Ty -> ST s ([MetaVar], Ty)
    gen' cxt@Cxt { llvl } (TMeta m) = do
      lookupMeta cxt m >>= \case
        Unsolved llvl' | llvl' > llvl -> pure ([m], TVar m)
        Solved _ ty -> gen' cxt ty
        _ -> pure ([], TMeta m)
    gen' cxt (TArrow a b) = do
      (bds , a) <- gen' cxt a
      (bds', b) <- gen' cxt b
      pure (nub (bds ++ bds'), TArrow a b)
    gen' cxt (TPair a b) = do
      (bds , a) <- gen' cxt a
      (bds', b) <- gen' cxt b
      pure (nub (bds ++ bds'), TPair a b)
    gen' cxt (TList a) = do (vars, a) <- gen' cxt a; pure (vars, TList a)
    gen' cxt TVoid = pure ([], TVoid)
    gen' cxt TFloat = pure ([], TFloat)
    gen' cxt TString = pure ([], TString)
    gen' cxt (TData nm tys) = do
      (bds, tys) <- mapAccumLM (\bds ty -> do
        (bds', ty) <- gen' cxt ty
        pure (bds ++ bds', ty)) [] tys
      pure (nub bds, TData nm tys)

-- wei have to make sure when trying to solve a meta originating from a type of or a type inside a let bound at a certain de-bruijn level with types of stuff originating from stuff later bound. solve let bounds by unification with types bound before. metas originating from types of bound lambda values (not of types originating from types of or inside let bounds) with lower de-bruijn levels can be solved by unification with any meta or type bound later but we can't solve metas stemming from let bounds with types stemming from later bound stuff to ensure let polymorphism. after all unifications the unsolved metas should be usable as type variables? all non type variables are solved (the solution can contain other metas (type variables after unification).

-- ALSO DON'T SOLVE METAS ORIGINATING FROM STUFF AT A CERTAIN DE-BRUIJN LEVEL WITH TYPE VARIABLES (UNSOLVED METAS) (SOMEHOW STORE PREVIOUS META. LET BOUNDS CAN HAVE SOLVED METAS) STEMMING ORIGINATING FROM LET BOUNDS WITH LOWER DE-BRUIJN LEVEL. type variables are metas that are TMETA n that force to "TMETA m"s after type checking. tmeta are the same when they force to the same solution

-- wei have to make sure wei don't solve metas originating from let bounds with unification results from types of values bound? at deeper levels. so wei have to pass at what level the type is wei're trying to unify and what's the de bruijn levels the original meta solution is from?
occurs :: MCxt s -> MetaVar -> Ty -> Result s ()
occurs mcxt m ty | trace ("occurs " ++ show m ++ " " ++ show ty ) False = undefined
occurs mcxt m (TMeta m') | m == m' = except $ Left "occurs failed"
occurs mcxt m (TPair a b) = occurs mcxt m a >> occurs mcxt m b
occurs mcxt m (TList a) = occurs mcxt m a
occurs mcxt m (TArrow a b) = occurs mcxt m a >> occurs mcxt m b
occurs mcxt m (TData _ tys) = foldrM (\a b -> occurs mcxt m a) () tys
occurs mcxt m _ = pure ()

unify :: Cxt s -> Ty -> Ty -> Result s ()
unify cxt l r = do
  res <- catchE (unify' cxt l r) (\e -> except $ Left $ "unifying " ++ show l ++ " with " ++ show r ++ " failed: " ++ e)
  _ <- trace ("unify (" ++ show l ++ ") (" ++ show r ++ ")") (pure ())
  pure res

writeMeta :: Cxt s -> MetaVar -> MetaEntry -> ST s ()
writeMeta cxt m e | trace ("writeMeta: " ++ show m ++ " " ++ show e) False = undefined
writeMeta cxt m (Solved _ (TMeta m')) | (m == m') = error "can't solve meta with itself"
writeMeta cxt m e = writeSTArray cxt.mcxt.metas m (Just e)

force :: Cxt s -> Int -> ST s (Either (Int, LetLvl) Ty)
force mcxt m = do
  lookupMeta mcxt m >>= \case
    Solved _ (TMeta m') -> force mcxt m' >>= \case
        Left (m', llvl) -> do writeMeta mcxt m (Solved Unmarked (TMeta m')); pure $ Left (m', llvl)
        Right ty -> do writeMeta mcxt m (Solved Unmarked ty); pure $ Right ty
    Solved _ ty -> do writeMeta mcxt m (Solved Unmarked ty); pure $ Right ty
    Unsolved llvl -> pure $ Left (m, llvl)


updateLvl :: Cxt s -> Int -> LetLvl -> ST s ()
updateLvl cxt m llvl = do
  lookupMeta cxt m >>= \case
    Unsolved llvl' | (llvl < llvl') -> writeMeta cxt m (Unsolved llvl')

checkCycles :: Cxt s -> MetaVar -> Int -> ST s (Either () ())
checkCycles cxt m marker = lookupMeta cxt m >>= \case
  Unsolved _ -> pure $ Right ()
  Solved (Marker marker') ty | (marker == marker') -> pure $ Left ()
  Solved _ ty -> case ty of
    (TArrow a b) -> undefined
    (TMeta m') -> undefined
    (TPair a b) -> undefined

unify' :: Cxt s -> Ty -> Ty -> Result s ()
unify' cxt l r = if l == r then pure () else do
  _ <- trace ("unify (" ++ show l ++ ") (" ++ show r ++ ")") (pure ())
  case (l, r) of
    (TArrow a b, TArrow a' b') -> unify cxt a a' *> unify cxt b b'
    (TPair a b, TPair a' b') -> unify cxt a a' *> unify cxt b b'
    (TList a, TList a') -> unify cxt a a'
    (TFloat, TFloat) -> pure ()
    (TString, TString) -> pure ()
    (TMaybe a, TMaybe a') -> unify cxt a a'
    (TBool, TBool) -> pure ()
    (TVoid, TVoid) -> pure ()
    (ty, ty') | (TMeta m) <- ty  -> do
      lift (lookupMeta cxt m) >>= \case
        Unsolved llvl -> undefined
        Solved _ ty' -> unify cxt ty' ty

      {-lift (force mcxt (TMeta m)) >>= \case
        TMeta m -> lift (force mcxt ty) >>= \case
          TMeta m' -> do
            if m == m' then return () else do
              ms <- lift $ lookupMeta mcxt m  
              ms' <- lift $ lookupMeta mcxt m'
              case (ms, ms') of
                (Unsolved llvl, Unsolved llvl') -> do
                  llvl <- pure $ min llvl llvl'
                  (m, m') <- pure (min m m', max m m')
                  lift $ writeMeta mcxt m $ Unsolved llvl
                  lift $ writeMeta mcxt m' $ Solved (TMeta m)
                (a, b) -> error $ "impossible: " ++ show a ++ " " ++ show b
          ty -> do
            occurs mcxt m ty
            solveMeta mcxt m ty
        ty' -> unify cxt ty' ty-}
    (ty        , TMeta a     ) -> unify cxt (TMeta a) ty
    (a         , a'          ) -> except $ Left $ "failed to unify " ++ show a ++ " with " ++ show a'

solveMeta :: MCxt s -> Int -> Marked -> Ty -> Result s ()
solveMeta mcxt m marker ty = do
  old <- lift (readSTArray (metas mcxt) m) >>= \case
    Just old -> pure old
    Nothing -> error "impossible"
  lift $ writeSTArray (metas mcxt) m $ Just $ Solved marker ty

check :: Cxt s -> Raw -> Ty -> Result s Tm
check = undefined

-- type Result s = StateT s (Either String)
type Result s = ExceptT String (ST s)

infer :: Cxt s -> Raw -> Result s Ty
infer cxt raw = do
  ty <- catchE (infer' cxt raw) (\err -> except $ Left ("infer " ++ show raw ++ " failed: " ++ "\n" ++ err))
  _ <- trace ("infer " ++ show raw ++ ": " ++ show ty) (pure True)
  pure ty

infer' :: Cxt s -> Raw -> Result s Ty
infer' cxt raw | trace ("infer' " ++ show raw) False = undefined
infer' cxt (RApp t u) = do
  a <- infer cxt u
  b <- lift $ freshMeta cxt
  t <- infer cxt t
  
  unify cxt (TArrow a b) t
  pure b
infer' cxt (RLam name tm) = do
  ty <- lift $ freshMeta cxt
  a <- lift $ freshMeta cxt
  cxt <- return $ bind cxt name a
  b <- infer cxt tm
  pure $ TArrow a b
infer' cxt (RVar name) = lookupType cxt name
infer' cxt (RLet name def scope) = do
  cxt <- return $ enterLet cxt
  ty <- lift $ freshMeta cxt
  cxt <- return $ bind cxt name ty
  ty' <- infer cxt def
  cxt <- return $ leaveLet cxt
  unify cxt ty ty'
  ty <- lift $ gen cxt ty
  cxt <- return $ bind cxt name ty
  infer cxt scope
infer' cxt (RLiteral lit) = pure $ inferLit lit

freshCxt :: ST s (Cxt s)
freshCxt = do
  mcxt <- newMCxt 
  pure $ Cxt {
    mcxt = mcxt,
    llvl = 1,
    env = []
  }

inferLit :: Literal -> Ty
inferLit lit = case lit of
  LitVoid -> TBuiltin PVoid
  LitString _ -> TBuiltin PString
  LitFloat _ -> TBuiltin PFloat
  LitInt _ -> TBuiltin PFloat

inferPrimOp :: PrimOp -> Ty
inferPrimOp Print = TArrow (TBuiltin PString) (TBuiltin PVoid)
inferPrimOp _ = TArrow (TBuiltin PFloat) (TBuiltin PFloat)
