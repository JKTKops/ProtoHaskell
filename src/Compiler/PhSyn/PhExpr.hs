module Compiler.PhSyn.PhExpr where

import Data.Text.Lazy (Text)

import Compiler.BasicTypes.SrcLoc (Located)
import Compiler.PhSyn.PhType

import Utils.Outputable

type LPhExpr id = Located (PhExpr id)

data PhExpr id
     = PhVar id

     | PhLit PhLit

     | PhLam (MatchGroup id (LPhExpr id))

     | PhApp (LPhExpr id) (LPhExpr id)

     | OpApp (LPhExpr id) -- left operand
             (LPhExpr id) -- operator
             (LPhExpr id) -- right operand

     | NegApp (LPhExpr id)

       -- Parenthesized expr
     | PhPar (LPhExpr id)

     | PhCase (LPhExpr id) (MatchGroup id (LPhExpr id))

     | PhIf (LPhExpr id) -- predicate
            (LPhExpr id) -- then
            (LPhExpr id) -- else

     | PhLet (LPhLocalBinds id) (LPhExpr id)

     | PhDo [LStmt id]

     | ExplicitTuple [LPhTupArg id]

     | ExplicitList [LPhExpr id]

     | ArithSeq (ArithSeqInfo id)

     | Typed (Type id) (LPhExpr id)
     deriving (Eq, Ord, Show)

data PhLit
     = LitInt Int
     | LitChar Char
     | LitString Text
     deriving (Eq, Ord, Show)

data MatchGroup id body
     = MG { mgAlts :: Located [LMatch id body]
          , mgCtxt :: MatchContext
          }
     deriving (Eq, Ord, Show)

type LMatch id body = Located (Match id body)
data Match id body
     = Match { matchPats :: [LPat id]
             , guardedRHSs :: [LGRHS id body]
             , localBinds :: LPhLocalBinds id -- 'where' clause
             }
     deriving (Eq, Ord, Show)

type LGRHS id body = Located (GRHS id body)
data GRHS id body = GRHS [LGuard id] body deriving (Eq, Ord, Show)

type LGuard id = Located (Guard id)
newtype Guard id = Guard [LPhExpr id] deriving (Eq, Ord, Show)

data MatchContext = FunCtxt | CaseCtxt deriving (Eq, Ord, Show, Enum, Bounded)

type LPat id = Located (Pat id)
data Pat id
     = PVar id
     | PCon id [Pat id]
     | PAs id (Pat id)
     | PLit PhLit
     | PWild
     deriving (Eq, Ord, Show)

type LPhLocalBinds id = Located (PhLocalBinds id)
newtype PhLocalBinds id = LocalBinds [LPhBind id]
  deriving (Eq, Ord, Show)

type LPhBind id = Located (PhBind id)
data PhBind id
     = VarBind id (LPhExpr id)
     | FunBind id (MatchGroup id (LPhExpr id))
     | PatBind (LPat id) (LPhExpr id)
     deriving (Eq, Ord, Show)

type LStmt id = Located (Stmt id)
data Stmt id
     = SExpr (LPhExpr id)                 -- ^ exp
     | SGenerator (LPat id) (LPhExpr id)  -- ^ pat <- exp
     | SLet (LPat id) (LPhExpr id)        -- ^ let pat = exp
     deriving (Eq, Ord, Show)

-- This will become more interesting if we implement TupleSections
type LPhTupArg id = LPhExpr id

data ArithSeqInfo id
     = From       (LPhExpr id)
     | FromThen   (LPhExpr id)
                  (LPhExpr id)
     | FromTo     (LPhExpr id)
                  (LPhExpr id)
     | FromThenTo (LPhExpr id)
                  (LPhExpr id)
                  (LPhExpr id)
     deriving (Eq, Ord, Show)
