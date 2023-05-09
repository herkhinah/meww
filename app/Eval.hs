module Eval where

import Syntax
import Syntax.Term (Tm)

data Val = Val | Neutral

newtype Env = List Val
