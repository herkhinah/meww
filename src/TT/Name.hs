module TT.Name where

import Control.DeepSeq (($!!))
import qualified Data.Text as T

data Name = UN !T.Text -- ^ User-provided name
          | MN !Int
          | NS !Name [T.Text] -- ^ Root, namespaces
          | UB -- ^ Unbound name
          deriving (Eq, Ord, Show)
          
txt :: String -> T.Text
txt = T.pack

-- Smart constructors for names, using old String style
sUN :: String -> Name
sUN s = UN (txt s)

sNS :: Name -> [String] -> Name
sNS n ss = NS n $!! (map txt ss)


