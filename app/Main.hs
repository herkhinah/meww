{-# OPTIONS -Wno-unused-imports #-}
module Main (main) where

--import Widgets
import Raw.Syntax
import Raw.Parser
import Compiler.ANF
import Check

import Control.Concurrent
import Foreign.C
import Control.Monad.ST
import Foreign
import Data.STRef
import Control.Monad.ST.Unsafe
import Data.IORef
import qualified Text.Megaparsec as P
import Control.Monad.Trans.Except (runExceptT, ExceptT)
import Control.Monad.Trans.Class

import System.Environment
import Type



main :: IO ()
main = do
  args <- getArgs
  files <- mapM (\x -> (x, ) <$> readFile x) args
  let results = map (uncurry parse) files
  mapM_ print results
