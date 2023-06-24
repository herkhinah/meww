module TT.Ctxt where

-- | Contexts used for global definitions and for proof state. They contain
-- universe constraints and existing definitions.
-- Also store maximum RigCount of the name (can't bind a name at multiplicity
-- 1 in a RigW, for example)
{-
data Context = MkContext {
                  next_tvar       :: Int,
                  definitions     :: Ctxt TTDecl
                } deriving (Show)

data Def = Function !Type !Term
         | TyDecl NameType !Type
         | Operator Type Int ([Value] -> Maybe Value)
         | CaseOp CaseInfo
                  !Type
                  ![(Type, Bool)] -- argument types, whether canonical
                  ![Either Term (Term, Term)] -- original definition
                  ![([Name], Term, Term)] -- simplified for totality check definition
                  !CaseDefs


type TTDecl = (Def, MetaInformation)
-}