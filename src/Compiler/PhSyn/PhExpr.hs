module Compiler.PhSyn.PhExpr where

import Data.Text.Lazy (Text)

import Compiler.BasicTypes.SrcLoc
import Compiler.PhSyn.PhType

import Utils.Outputable

type LPhExpr id = Located (PhExpr id)
data PhExpr id
     = PhVar id

     | PhLit PhLit

     | PhLam (MatchGroup id)

     | PhApp (LPhExpr id) (LPhExpr id)

     | OpApp (LPhExpr id) -- left operand
             (LPhExpr id) -- operator, ALWAYS a PhVar
             (LPhExpr id) -- right operand

     | NegApp (LPhExpr id)

       -- Parenthesized expr, see NOTE: [Par constructors in syn]
     | PhPar (LPhExpr id)

     | PhCase (LPhExpr id) (MatchGroup id)

     | PhIf (LPhExpr id) -- predicate
            (LPhExpr id) -- then
            (LPhExpr id) -- else

     | PhLet (LPhLocalBinds id) (LPhExpr id)

     | PhDo [LStmt id]

     | ExplicitTuple [LPhTupArg id]

     | ExplicitList [LPhExpr id]

     | ArithSeq (ArithSeqInfo id)

     | Typed (LPhType id) (LPhExpr id)
     deriving (Eq, Ord, Show)

mkLPhAppExpr :: LPhExpr id -> LPhExpr id -> LPhExpr id
mkLPhAppExpr e1@(Located s1 _) e2@(Located s2 _) =
    Located (combineSrcSpans s1 s2) (PhApp e1 e2)

data PhLit
     = LitInt Integer
     | LitFloat Double
     | LitChar Char
     | LitString Text
     deriving (Eq, Ord, Show)

data MatchGroup id
     = MG { mgAlts :: [LMatch id]
          , mgCtxt :: MatchContext
          }
     deriving (Eq, Ord, Show)

type LMatch id = Located (Match id)
data Match id
     = Match { matchPats :: [LPat id]
             , rhs :: LRHS id
             , localBinds :: Maybe (LPhLocalBinds id) -- 'where' clause
             }
     deriving (Eq, Ord, Show)

type LRHS id = Located (RHS id)
data RHS id
     = Unguarded (LPhExpr id)
     | Guarded [LGuard id]
     deriving (Eq, Ord, Show)

type LGuard id = Located (Guard id)
data Guard id = Guard (LPhExpr id) (LPhExpr id)
  deriving (Eq, Ord, Show)

data MatchContext
     = FunCtxt
     | CaseCtxt
     | LamCtxt
     | LetCtxt
     deriving (Eq, Ord, Show, Enum, Bounded)

isCaseOrLamCtxt :: MatchContext -> Bool
isCaseOrLamCtxt CaseCtxt = True
isCaseOrLamCtxt LamCtxt  = True
isCaseOrLamCtxt _        = False

type LPat id = Located (Pat id)
data Pat id
     = PVar id
     | PCon id [Pat id]
     | PAs id (Pat id)
     | PLit PhLit
     | PWild
     | PTuple [Pat id]
     | PList  [Pat id]
     | ParPat (Pat id) -- Parenthesized pattern, see NOTE: [Par constructors in syn]
     deriving (Eq, Ord, Show)

type LPhLocalBinds id = Located (PhLocalBinds id)
data PhLocalBinds id = LocalBinds [LPhBind id] [LSig id]
  deriving (Eq, Ord, Show)

type LPhBind id = Located (PhBind id)
data PhBind id
     = FunBind id (MatchGroup id)    -- ^ f x = e
                                     -- id = f, mg = ([PVar x], body = b)
     | PatBind (LPat id) (LRHS id)   -- ^ Just x = e
                                     -- Pattern bindings include x = 5
     deriving (Eq, Ord, Show)

type LStmt id = Located (Stmt id)
data Stmt id
     = SExpr (LPhExpr id)                 -- ^ exp
     | SGenerator (LPat id) (LPhExpr id)  -- ^ pat <- exp
     | SLet (LPhLocalBinds id)            -- ^ let bindings
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


{- NOTE: [Par constructors in syn]

To reduce headache:

* The pretty printer does NOT add parens, except for PhPar.

In general when printing back what the user wrote, we will use locations and
print directly from the source code. But when pretty printing generated expressions,
we simply ensure that we generate the correct PhPar wrappers.

-}

type LSig id = Located (Sig id)
data Sig id
     = TypeSig [id] (LPhType id)
     | FixitySig Assoc Int [id]
     deriving (Eq, Ord, Show)

data Assoc = Infix | InfixL | InfixR deriving (Eq, Ord, Show, Enum, Bounded)


----------------------------------------------------------------------------------
-- Outputable instances
----------------------------------------------------------------------------------

instance Outputable id => Outputable (PhExpr id) where
    ppr (PhVar id)        = ppr id
    ppr (PhLit lit)       = ppr lit
    ppr (PhLam mg)        = ppr mg
    ppr (PhApp e1 e2)     = ppr e1 <+> ppr e2
    ppr (OpApp e1 e2 e3)  = ppr e1 <+> ppr e2 <+> ppr e3
    ppr (NegApp e)        = text "-" <> ppr e
    ppr (PhPar e)         = parens $ ppr e
    ppr (PhCase scrut mg) = text "case" <+> ppr scrut <+> text "of"
                            $$ nest 2 (ppr mg)
    ppr (PhIf p t f)      = text "if" <+> ppr p
                            <+> text "then" <+> ppr t
                            <+> text "else" <+> ppr f
    ppr (PhLet binds e)   = text "let" <+> nest 4 (ppr binds)
                            $$ text "in" <+> ppr e
    ppr (PhDo stmts)      = text "do" <+> nest 3 (vcat $ map ppr stmts)
    ppr (ExplicitTuple tupArgs) = parens $ hcat $ punctuate comma $ map ppr tupArgs
    ppr (ExplicitList elems) = brackets $ hcat $ punctuate comma $ map ppr elems
    ppr (ArithSeq info) = brackets $ ppr info
    ppr (Typed t e)     = ppr e <+> text "::" <+> ppr t

instance Outputable PhLit where
    ppr (LitInt i) = integer i
    ppr (LitFloat d) = double d
    ppr (LitChar c) = char '\'' <> char c <> char '\''
    ppr (LitString s) = text $ show s

instance Outputable id => Outputable (MatchGroup id) where
    ppr (MG (map unLoc -> alts) ctxt) = vcat $ map (pprMatch ctxt) alts

pprMatch :: Outputable id => MatchContext -> Match id -> Doc
pprMatch ctxt (Match pats rhs locals) =
  hsep (map ppr pats) <+> pprRhs (if isCaseOrLamCtxt ctxt then text "->" else text "=") rhs
                      $$ pprLocals locals

pprLocals :: Outputable id => Maybe (LPhLocalBinds id) -> Doc
pprLocals Nothing = mempty
pprLocals (Just (unLoc -> ls)) = nest 2 $ text "where" $$ nest 2 (ppr ls)

pprRhs :: Outputable id => Doc -> LRHS id -> Doc
pprRhs ctxt (unLoc -> Unguarded body) = ctxt <+> ppr body
pprRhs ctxt (unLoc -> Guarded guards) = nest 2 $ vcat $ map (pprGuard ctxt) guards

pprGuard :: Outputable id => Doc -> LGuard id -> Doc
pprGuard ctxt (unLoc -> Guard guard body) = text "|" <+> ppr guard <+> ctxt <+> ppr body

instance Outputable id => Outputable (Pat id) where
    ppr (PVar id) = ppr id
    ppr (PCon id args) = ppr id <+> hsep (map ppr args)
    ppr (PAs id pat) = ppr id <> char '@' <> ppr pat
    ppr (PLit lit)   = ppr lit
    ppr PWild        = char '_'
    ppr (PTuple ps)  = parens $ fsep $ punctuate comma $ map ppr ps
    ppr (PList ps)   = brackets . fsep . punctuate comma $ map ppr ps
    ppr (ParPat pat) = parens $ ppr pat

instance Outputable id => Outputable (PhLocalBinds id) where
    ppr (LocalBinds binds sigs) = vcat (map ppr binds ++ map ppr sigs)

instance Outputable id => Outputable (PhBind id) where
    ppr (FunBind id mg)   = ppr id <+> ppr mg
    ppr (PatBind pat body) = ppr pat <+> char '=' <+> pprRhs (text "=") body

instance Outputable id => Outputable (Stmt id) where
    ppr (SExpr e) = ppr e
    ppr (SGenerator pat e) = ppr pat <+> text "<-" <+> ppr e
    ppr (SLet binds) = text "let" <+> nest 4 (ppr binds)

instance Outputable id => Outputable (ArithSeqInfo id) where
    ppr (From e) = ppr e <+> text ".."
    ppr (FromThen e1 e2) = (ppr e1 <> comma) <+> ppr e2 <+> text ".."
    ppr (FromTo e1 e2) = ppr e1 <+> text ".." <+> ppr e2
    ppr (FromThenTo e1 e2 e3) =
        (ppr e1 <> comma) <+> ppr e2 <+> text ".." <+> ppr e3

instance Outputable id => Outputable (Sig id) where
    ppr (TypeSig id t) = ppr id <+> text "::" <+> ppr t
    ppr (FixitySig assoc prec ids) = ppr assoc
                                     <+> int prec
                                     <+> hcat (punctuate comma $ map ppr ids)

instance Outputable Assoc where
    ppr Infix  = text "infix"
    ppr InfixL = text "infixl"
    ppr InfixR = text "infixr"

