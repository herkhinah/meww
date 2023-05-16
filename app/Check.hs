module Check where

import Common (Name)
import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Data.IORef
import Data.List (elemIndex, find, findIndex)
import Data.Map
import Data.STRef
import GHC.Arr (STArray, newSTArray, readSTArray, writeSTArray)
import GHC.IO (unsafeDupablePerformIO)
import Syntax

-- we want don't want to unify the types inside let bounds with types of bounds with deeper level
type MetaVar = Int

type TypeVar = Int

type LetLvl = Int

data MetaEntry = Solved Ty LetLvl | Unsolved LetLvl

data MCxt s = MCxt
  { metas :: STArray s Int (Maybe MetaEntry),
    nextMeta :: STRef s Int
  }

newMCxt :: ST s (MCxt s)
newMCxt = do
  metas <- newSTArray (0, 8192) Nothing
  nextMeta <- newSTRef 0
  pure MCxt {metas = metas, nextMeta = nextMeta}

freshMeta :: MCxt s -> LetLvl -> ST s Ty
freshMeta mcxt llvl = do
  m <- readSTRef $ nextMeta mcxt
  writeSTRef (nextMeta mcxt) (m + 1)
  writeSTArray (metas mcxt) m $ Just $ Unsolved llvl
  pure $ TMeta m

-- type Result s a = ExceptT String (ST (MCxt s))

lookupMeta :: MCxt s -> MetaVar -> ST s MetaEntry
lookupMeta mcxt m = do
  m <- readSTArray (metas mcxt) m
  case m of
    Just m -> pure m
    Nothing -> error "impossible"

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
  deriving (Show)

data TBuiltin = TString | TFloat | TVoid deriving (Show)

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

data Cxt = Cxt
  { types :: [Ty],
    names :: [Name],
    bds :: [BD], -- wei don't want to unify types of let definitions with the types of later bound/defined values
    env :: [Val],
    llvl :: LetLvl
  }

define :: Cxt -> Name -> Val -> Ty -> Cxt
define cxt nm val ty =
  Cxt
    { types = ty : types cxt,
      names = nm : names cxt,
      bds = Defined : bds cxt,
      env = val : env cxt,
      llvl = 1 + llvl cxt
    }

bind :: Cxt -> Name -> Ty -> (Cxt, Tm)
bind cxt nm tm ty =
  let
  Cxt
    { types = ty : types cxt,
      names = nm : names cxt,
      bds = Bound : bds cxt,
      env = tm : env cxt,
      llvl = llvl cxt
    }

lookupType :: MCxt s -> Cxt -> Name -> Result s Ty
lookupType mcxt cxt name = do
  ix <- undefined -- elemIndex name (reverse $ names cxt)
  pure $ types cxt !! ix

force :: MCxt s -> Ty -> Ty
force mcxt ty = undefined

infixl 8 $$

($$) :: Closure -> Val -> Val
($$) (Closure env t) u = eval (u : env) t

-- wei have to make sure when trying to solve a meta originating from a type of or a type inside a let bound at a certain de-bruijn level with types of stuff originating from stuff later bound. solve let bounds by unification with types bound before. metas originating from types of bound lambda values (not of types originating from types of or inside let bounds) with lower de-bruijn levels can be solved by unification with any meta or type bound later but we can't solve metas stemming from let bounds with types stemming from later bound stuff to ensure let polymorphism. after all unifications the unsolved metas should be usable as type variables? all non type variables are solved (the solution can contain other metas (type variables after unification).

-- ALSO DON'T SOLVE METAS ORIGINATING FROM STUFF AT A CERTAIN DE-BRUIJN LEVEL WITH TYPE VARIABLES (UNSOLVED METAS) (SOMEHOW STORE PREVIOUS META. LET BOUNDS CAN HAVE SOLVED METAS) STEMMING ORIGINATING FROM LET BOUNDS WITH LOWER DE-BRUIJN LEVEL. type variables are metas that are TMETA n that force to "TMETA m"s after type checking. tmeta are the same when they force to the same solution

-- wei have to make sure wei don't solve metas originating from let bounds with unification results from types of values bound? at deeper levels. so wei have to pass at what level the type is wei're trying to unify and what's the de bruijn levels the original meta solution is from?

unify :: MCxt s -> Cxt -> Ty -> Ty -> Result s ()
unify mcxt cxt (TArrow a b) (TArrow a' b') = unify mcxt cxt a a' >> unify mcxt cxt b b'
unify mcxt cxt (TMeta a) (TMeta b) = undefined
unify mcxt cxt a a' = except $ Left $ "failed to unify " ++ show a ++ "with" ++ show a'

check :: MCxt s -> Cxt -> Raw -> Ty -> Result s Tm
check = undefined

-- type Result s = StateT s (Either String)
type Result s = ExceptT String (ST s)

infer :: MCxt s -> Cxt -> Raw -> Result s (Tm, Ty)
infer mcxt cxt (RApp t u) = do
  (t, tty) <- infer mcxt cxt t
  (a, b) <- case force mcxt tty of
    TArrow a b -> pure (a, b)
    tty -> do
      a <- lift $ freshMeta mcxt (llvl cxt)
      b <- lift $ freshMeta mcxt (llvl cxt)
      unify mcxt cxt (TArrow a b) tty
      pure (a, b)
  u <- check mcxt cxt u a
  pure (App t u, b)
infer mcxt cxt (RLam name tm) = do
  ty <- lift $ freshMeta mcxt (llvl cxt)
  (cxt, a) <- lift $ bind mcxt cxt name
  (t, b) <- infer mcxt cxt tm
  pure (Lam t, TArrow a b)
infer mcxt cxt (RVar name) = do
  ty <- lookupType mcxt cxt name
  pure (undefined, ty)
infer mcxt cxt (RLet name def scope) = do
  (tm, ty) <- infer mcxt cxt def
  cxt <- define mcxt cxt name tm ty
  infer mcxt cxt scope
infer mcxt cxt (RLiteral lit) = pure (Lit lit, inferLit lit)
infer mcxt cxt (RPrimOp op) = pure (PrimOp op, inferPrimOp op)

inferLit :: Literal -> Ty
inferLit lit = case lit of
  LitVoid -> TBuiltin TVoid
  LitString _ -> TBuiltin TString
  LitFloat _ -> TBuiltin TFloat
  LitInt _ -> TBuiltin TFloat

inferPrimOp :: PrimOp -> Ty
inferPrimOp Print = TArrow (TBuiltin TString) (TBuiltin TVoid)
inferPrimOp _ = TArrow (TBuiltin TFloat) (TBuiltin TFloat)
