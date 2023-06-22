{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Main (main) where

import Raw.Syntax
import Raw.Parser
import Check.Check
import Check.Cxt

import Control.Monad.ST
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class


import System.Environment
import Debug.Trace

check :: [Toplevel] -> Either String (IO ())
check defs = runST $ runExceptT $ do
  cxt <- lift mkCxt
  cxt <- declareToplevelFuns cxt defs
  let p1 = print cxt
  trace (show cxt._globals) (pure ())
  cxt <- typecheckToplevelFuns cxt
  pure $ p1 >> print cxt



main :: IO ()
main = do
  args <- getArgs
  files <- mapM (\x -> (x, ) <$> readFile x) args
  let Right (Right rawDefs) = sequence <$> mapM (fmap check . uncurry parse) files
  [()] <- sequence rawDefs
  pure ()
