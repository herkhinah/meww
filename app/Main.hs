{-# OPTIONS -Wno-unused-imports #-}
module Main (main) where

import Widgets
import Syntax
import Parser
import Check
import Eval


import Control.Concurrent
import Foreign.C
import Control.Monad.ST
import Foreign
import Data.STRef
import Control.Monad.ST.Unsafe
import Data.IORef
import qualified Text.Megaparsec
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Class

{-
counter :: ST s CInt -> ST s CInt
counter x = do
  x' <- x
  pure (x' + 1)
-}


foreign import ccall "wrapper"
  mkCallback :: (IO CInt) -> IO (FunPtr (IO CInt))

ffiCounter :: IORef CInt -> IO (FunPtr (IO CInt))
ffiCounter ref = mkCallback $ do
  val <- readIORef ref
  modifyIORef ref (+ 1)
  return val


example1 = "(let (map (lam (f m) (if (empty m) nil (cons (f (head m)) (map f (tail m)))))) map)"

main :: IO ()
main = do
  let Right raw = Text.Megaparsec.parse (parens pLet) "" example1
  let ty = runST (runExceptT (do cxt <- lift freshCxt; infer cxt raw))
  putStr (show ty)
  pure ()
