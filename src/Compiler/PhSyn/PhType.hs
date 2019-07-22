module Compiler.PhSyn.PhType where

import Data.String

import Compiler.BasicTypes.SrcLoc
import Utils.Outputable

type LPhType id = Located (PhType id)
data PhType id
       -- Type variable or type constructor
     = PhVarTy id

       -- Context => type
     | PhQualTy [Pred id]    -- context (C in C => A)
                (LPhType id) -- payload (A in C => A)

     | PhAppTy (LPhType id) (LPhType id)

     | PhFunTy (LPhType id) (LPhType id)

     | PhListTy (LPhType id)

     | PhTupleTy [LPhType id]

     | PhParTy (LPhType id) -- parenthesized type
       -- See NOTE: [Par constructors in syn] in Compiler/PhSyn/PhExpr

     deriving (Show, Eq, Ord)

data Pred id = IsIn id (PhType id) -- Eq a, Eq [a] etc.
  deriving (Eq, Ord, Show)

instance Outputable id => Outputable (PhType id) where
    ppr (PhVarTy id) = ppr id
    ppr (PhQualTy ctxt t) = case ctxt of
        [] -> ppr t
        [c] -> ppr c <+> text "=>" <+> ppr t
        cs  -> parens (sep (punctuate comma $ map ppr cs)) <+> text "=>" <+> ppr t
    ppr (PhAppTy t1 t2) = ppr t1 <+> ppr t2
    ppr (PhFunTy t1 t2) =  ppr t1 <+> text "->" <+> ppr t2
    ppr (PhListTy t) = brackets $ ppr t
    ppr (PhTupleTy ts) = parens $ sep $ punctuate comma $ map ppr ts
    ppr (PhParTy t) = parens $ ppr t

instance Outputable id => Outputable (Pred id) where
    ppr (IsIn id t) = ppr id <+> ppr t

mkLPhFunTy :: LPhType id -> LPhType id -> LPhType id
mkLPhFunTy t1@(Located span1 _) t2@(Located span2 _) =
    Located (combineSrcSpans span1 span2) (PhFunTy t1 t2)

mkLPhAppTy :: LPhType id -> LPhType id -> LPhType id
mkLPhAppTy t1@(Located span1 _) t2@(Located span2 _) =
    Located (combineSrcSpans span1 span2) (PhAppTy t1 t2)


