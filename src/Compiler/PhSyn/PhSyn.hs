module Compiler.PhSyn.PhSyn where

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T

import Compiler.BasicTypes.SrcLoc (Located, unLoc)

import Compiler.PhSyn.PhExpr
import Compiler.PhSyn.PhType

import Utils.Outputable

data PhModule a = Module
  { modName :: Maybe (Located Text)
  , modDecls :: [LPhDecl a]
  }
  deriving (Eq, Ord, Show)

type LPhDecl a = Located (PhDecl a)
data PhDecl id
     = Binding (PhBind id)
     | Signature (Sig id)
     | DataDecl id [id] [ConDecl id]
     | ClassDecl [Pred id]          -- Superclasses
                 id                 -- Class name
                 id                 -- the 'a' in 'class Eq a where'
                 (LPhLocalBinds id) -- The class functions and signatures

     | InstDecl [Pred id]          -- Context
                id                 -- Class name
                (LPhType id)        -- the '[a]' in 'instance Eq [a] where'
                (LPhLocalBinds id) -- The class function implementations
     deriving (Eq, Ord, Show)

-- This will become more interesting when we add records
data ConDecl id
     = ConDecl id [PhType id]
  deriving (Eq, Ord, Show)

instance Outputable b => Outputable (PhModule b) where
    ppr (Module Nothing decls) = vcat (map ppr decls)
    ppr (Module (Just name) decls) =
        vcat $ (text "module" <+> text (T.unpack $ unLoc name) <+> text "where") :
        map ppr decls

instance Outputable id => Outputable (PhDecl id) where
    ppr (Binding binding) = ppr binding
    ppr (Signature sig)   = ppr sig
    ppr (DataDecl name tyvars (c:cs)) = text "data"
                                         <+> ppr name
                                         <+> hsep (map ppr tyvars)
                                         $$ nest 5 (vcat cons)
      where
        cons :: [Doc]
        cons = (text "=" <+> ppr c) : prepend (text "|") (map ppr cs)
        prepend :: Doc -> [Doc] -> [Doc]
        prepend d = map (d <+>)

    -- TODO fix the bindings part of the following two
    ppr (ClassDecl scs name tyvar binds) =
        let pscs = case scs of
                [] -> mempty
                [sc] -> ppr sc <+> text "=>"
                scs  -> (parens . hsep . punctuate comma $ map ppr scs) <+> text "=>"
        in text "class" <+> pscs <+> ppr name <+> ppr tyvar <+> text "where"
                        $$ nest 4 (ppr binds)
    ppr (InstDecl prds name head binds) =
        let pprds = case prds of
                [] -> mempty
                [prd] -> ppr prd <+> text "=>"
                prds  -> (parens . hsep . punctuate comma $ map ppr prds) <+> text "=>"
        in text "instance" <+> pprds <+> ppr name <+> ppr head <+> text "where"
                           $$ nest 4 (ppr binds)

instance Outputable id => Outputable (ConDecl id) where
    ppr (ConDecl name argTypes) = ppr name <+> hsep (map ppr argTypes)
