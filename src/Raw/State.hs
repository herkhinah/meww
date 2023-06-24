module Raw.State
  ( MewwState (..),
    initialMewwState
  )
where

import qualified Data.Text as T
import qualified Data.Map.Strict as M


data MewwState = MewwState
  {   brace_stack :: [Maybe Int]
    , indent_stack :: [Int]
    , syntax_keywords :: [String]
    , module_aliases :: M.Map [T.Text] [T.Text]
  }

initialMewwState :: MewwState
initialMewwState = MewwState [] [] [] M.empty