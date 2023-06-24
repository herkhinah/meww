module Raw.Parser.FC 
(
  FC(..)
)
where


import Prelude (Bool(..), Eq(..), Int, Monoid(..), Ord(..), Show(..),
                String, max, min, otherwise, (&&), (||))

import qualified Prelude as S (Semigroup(..))


import Data.Data (Data)
import Data.List ( (++) )
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

-- | Source location. These are typically produced by 'Idris.Parser.Stack.withExtent'
data FC = FC { _fc_fname :: String, -- ^ Filename
               _fc_start :: (Int, Int), -- ^ Line and column numbers for the start of the location span
               _fc_end :: (Int, Int) -- ^ Line and column numbers for the end of the location span
             }
        | NoFC -- ^ Locations for machine-generated terms
        | FileFC { _fc_fname :: String } -- ^ Locations with file only
  deriving (Data, Generic, Typeable, Ord)

-- TODO: find uses and destroy them, doing this case analysis at call sites
-- | Give a notion of filename associated with an FC
fc_fname :: FC -> String
fc_fname (FC f _ _) = f
fc_fname NoFC = "(no file)"
fc_fname (FileFC f) = f

-- TODO: find uses and destroy them, doing this case analysis at call sites
-- | Give a notion of start location associated with an FC
fc_start :: FC -> (Int, Int)
fc_start (FC _ start _) = start
fc_start NoFC = (0, 0)
fc_start (FileFC f) = (0, 0)

-- TODO: find uses and destroy them, doing this case analysis at call sites
-- | Give a notion of end location associated with an FC
fc_end :: FC -> (Int, Int)
fc_end (FC _ _ end) = end
fc_end NoFC = (0, 0)
fc_end (FileFC f) = (0, 0)

instance S.Semigroup FC where
  -- | Get the largest span containing the two FCs
  (<>) (FC f start end) (FC f' start' end')
      | f == f' = FC f (min start start') (max end end')
      | otherwise = NoFC
  (<>) fc@(FC f _ _) (FileFC f') | f == f' = fc
                                    | otherwise = NoFC
  (<>) (FileFC f') fc@(FC f _ _) | f == f' = fc
                                    | otherwise = NoFC
  (<>) (FileFC f) (FileFC f') | f == f' = FileFC f
                                 | otherwise = NoFC
  (<>) NoFC fc = fc
  (<>) fc NoFC = fc

instance Monoid FC where
  mempty = NoFC

  

-- | Determine whether the first argument is completely contained in the second
fcIn :: FC -> FC -> Bool
fcIn NoFC   _ = False
fcIn (FileFC _) _ = False
fcIn (FC {}) NoFC = False
fcIn (FC {}) (FileFC _) = False
fcIn (FC fn1 (sl1, sc1) (el1, ec1)) (FC fn2 (sl2, sc2) (el2, ec2)) =
  fn1 == fn2 &&
  (sl1 == sl2 && sc1 > sc2 || sl1 > sl2) &&
  (el1 == el2 && ec1 < ec2 || el1 < el2)

-- | Ignore source location equality (so deriving classes do not compare FCs)
instance Eq FC where
  _ == _ = True

-- | FC with equality
newtype FC' = FC' { unwrapFC :: FC } deriving (Data, Generic, Typeable, Ord)

instance Eq FC' where
  FC' fc == FC' fc' = fcEq fc fc'
    where fcEq (FC n s e) (FC n' s' e') = n == n' && s == s' && e == e'
          fcEq NoFC NoFC = True
          fcEq (FileFC f) (FileFC f') = f == f'
          fcEq _ _ = False

instance Show FC' where
  showsPrec d (FC' fc) = showsPrec d fc

-- | Empty source location
emptyFC :: FC
emptyFC = NoFC

-- | Source location with file only
fileFC :: String -> FC
fileFC s = FileFC s

{-!
deriving instance Binary FC
!-}


instance Show FC where
    show (FC f s e) = f ++ ":" ++ showLC s e
      where showLC (sl, sc) (el, ec) | sl == el && sc == ec = show sl ++ ":" ++ show sc
                                     | sl == el             = show sl ++ ":" ++ show sc ++ "-" ++ show ec
                                     | otherwise            = show sl ++ ":" ++ show sc ++ "-" ++ show el ++ ":" ++ show ec
    show NoFC = "No location"
    show (FileFC f) = f