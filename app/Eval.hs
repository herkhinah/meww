module Eval where

import Syntax

data Val = Val | Neutral

newtype Env = List Val
