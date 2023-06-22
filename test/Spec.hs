{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}


import Raw.Parser (parse, pLetRec, pLam)
import Type (Ty)
import Raw.Syntax (Raw(..), Toplevel)
import Check.Check
import Check.Globals
import Check.Cxt


import Control.Monad.ST
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Data.Map.Strict ((!?), insert)
import Data.Graph
import Debug.Trace (trace)
import qualified Data.Bifunctor
import Common (Name)
import Data.Map ((!))
import qualified Text.Megaparsec as Mp

lookupVertex :: Globals -> Name ->  Maybe Vertex
lookupVertex globals nm = lookup nm globals._vertices

buildGraph :: Globals -> [Vertex] -> [(String, [Vertex])] ->  Raw -> [Vertex]
buildGraph = buildGraph'

buildGraph' :: Globals -> [Vertex] -> [(String, [Vertex])] ->  Raw -> [Vertex]
buildGraph' cxt out env (RVar nm) = case lookup nm env of
  Just vertices -> out ++ vertices
  Nothing ->
    case lookupVertex cxt nm of
      Just v -> out ++ [v]
      Nothing -> out
buildGraph' cxt out env (RApp rator rands) = foldr (\rand out -> out ++ buildGraph cxt out env rand) out rands ++ buildGraph cxt out env rator
buildGraph' cxt out env (RLet defs scope) = buildGraph cxt out (foldl (\env def -> Data.Bifunctor.second (buildGraph cxt out env) def : env) env defs) scope
buildGraph' cxt out env (RLam nms scope) = buildGraph cxt out (foldl (\env nm -> (nm, []) : env) env nms) scope
buildGraph' _   out _   _ = out


example1 :: String
example1 = "letrec (map (lam (f m) (if (empty m) nil (cons (f (head m)) (map f (tail m)))))) map"
example2 :: String
example2 = "lam (x y) (let (x (x y)) (lam x (y x)))"

example3 :: String
example3 = "letrec\
\    ((even (lam (o e n) (if (eq n 0) e (odd o e (- n 1)))))\
\     (odd (lam (o e n) (if (eq n 0) o (even o e (- n 1))))))\
\    (even true false 3)"


example4 :: String
example4 = "(data Bool () (True) (False))\
\(fun even (o e n) (if (eq n 0) e (odd o e (- n 1))))\
\(fun odd (o e n) (if (eq n 0) o (even o e (- n 1))))\
\(fun foo () (even true false 3))\
\(fun bar (a) (even a False 4))"

type Result s = ExceptT String (ST s)

runExample :: Raw -> Either String (Raw, Ty)
runExample raw = runST $ runExceptT $ do
  cxt <- lift mkCxt
  (tm, ty) <- infer cxt raw
  (, ) <$> lift (forceTm cxt tm) <*> lift (force cxt ty)


runToplevelExample :: [Toplevel] -> Either String Globals
runToplevelExample defs = runST $ runExceptT $ do
  cxt <- lift mkCxt
  cxt <- declareToplevelFuns cxt defs
  cxt <- declareToplevelData cxt defs
  trace (show cxt) (pure ())
  let m = (\(nm, vertex) -> case cxt._globals._funs !? nm of
                                Just (def, _) -> (nm, vertex, buildGraph cxt._globals [] [] def)
                                Nothing -> (nm, vertex, [])) :: (String, Vertex) -> (String, Vertex, [Vertex])
  let out = fmap m cxt._globals._vertices
  let sccs = stronglyConnComp out
  cxt <- inferTopLevels sccs cxt
  pure (_globals cxt)


inferTopLevels :: [SCC String] -> Cxt s -> Result s (Cxt s)
inferTopLevels (CyclicSCC nms : rem) cxt = do
  defs <- pure $ map (\nm -> (nm, cxt._globals._funs ! nm)) nms
  tys <- mapM (\(_, (raw, _)) -> infer (enterLet cxt) raw) defs
  defs <- mapM (\((nm, (_, ty)), (raw, ty')) -> (nm, ) . (raw, ) <$> (unify cxt ty ty' >>= \x -> lift (gen cxt x))) (zip defs tys)
  inferTopLevels rem (cxt { _globals = cxt._globals { _funs = foldl (\funs (nm, (raw, ty)) -> insert nm (raw, ty) funs) cxt._globals._funs defs}})
inferTopLevels (AcyclicSCC nm : rem) cxt = do
  (raw, ty) <- pure $ cxt._globals._funs ! nm
  (raw, ty') <- infer cxt raw
  ty <- unify cxt ty ty' >>= \ty -> lift $ gen cxt ty
  inferTopLevels rem cxt { _globals = cxt._globals { _funs = insert nm (raw, ty) cxt._globals._funs }}
inferTopLevels [] cxt = pure cxt

main :: IO ()
main = do
  
    let Right ex1 = Mp.parse pLetRec "example1" example1
    print $ runExample ex1

    let Right ex2 = Mp.parse pLam "example2" example2
    print $ runExample ex2

    let Right ex3 = Mp.parse pLetRec "example3" example3
    print ex3
    case runExample ex3 of
      Right (tm3, ty3) -> print tm3 >> print ty3
      Left err -> print err
  
    let Right ex4 = Raw.Parser.parse "example4" example4
    print ex4
    print ex4

    case runToplevelExample ex4 of
      Right foo -> print foo
      Left err -> print err
    pure ()