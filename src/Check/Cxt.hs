{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Check.Cxt where
import Control.Monad.ST
import GHC.Arr (STArray, newSTArray)
import Data.STRef (STRef, newSTRef)
import Type (Ty)
import Common (Name)
import Check.Unify
import GHC.Records.Compat(HasField(..))

import Control.Lens
import Check.Globals
import qualified Data.Map.Strict as M


data Cxt s = Cxt {
  _sUnify :: SUnify s,
  _env :: [(Name, Ty)],
  _globals :: Globals
}
 
$(makeLenses ''Cxt )

instance Show (Cxt s) where
  show = show . view globals


mkCxt :: ST s (Cxt s)
mkCxt = do
  _metas <- newSTArray (0, 8192) Nothing
  _nextMeta <- newSTRef 0
  pure $ Cxt {
    _sUnify = SUnify { _metas, _nextMeta, _llvl = 1 },
    _env = [],
    _globals = Globals { _vertices = [], _edges = [], _conss = M.empty, _funs = M.empty, ___types = M.empty }
  }
