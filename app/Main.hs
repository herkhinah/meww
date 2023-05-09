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
  
  


main :: IO ()
main = do
  counter <- newIORef 0
  counter <- ffiCounter counter
  printNum counter
  printNum counter
  printNum counter
  printNum counter
  run
  name <- newCString "foobar"
  createWindow name
  threadDelay 10000000
  pure ()
