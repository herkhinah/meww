import Raw.Parser (pLam, pLet, pLetRec)
import Type (Ty)
import Raw.Syntax (Raw)

import Text.Megaparsec (parse)
import Control.Monad.ST
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Check (mkCxt, infer, force, forceTm)

example1 = "letrec (map (lam (f m) (if (empty m) nil (cons (f (head m)) (map f (tail m)))))) map"
example2 = "lam (x y) (let (x (x y)) (lam x (y x)))"

example3 = "letrec\
\    ((even (lam n (if (eq n 0) true (odd (- n 1)))))\
\     (odd (lam n (if (eq n 0) false (even (- n 1))))))\
\    (even 3)"

type Result s = ExceptT String (ST s)

runExample :: Raw -> Either String (Raw, Ty)
runExample raw = runST $ runExceptT $ do
  cxt <- lift mkCxt
  (tm, ty) <- infer cxt raw
  (, ) <$> lift (forceTm cxt tm) <*> lift (force cxt ty)
 

main :: IO ()
main = do
    let Right ex1 = parse pLetRec "example1" example1
    print $ runExample ex1

    let Right ex2 = parse pLam "example2" example2
    print $ runExample ex2

    let Right ex3 = parse pLetRec "example3" example3
    let Right (tm3, ty3) = runExample ex3
    print tm3
    print ty3

    pure ()