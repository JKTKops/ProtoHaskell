module Compiler.PhSyn.PhSyn where

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T

import Compiler.BasicTypes.SrcLoc (Located)

import Compiler.PhSyn.PhExpr
import Compiler.PhSyn.PhType

import Utils.Outputable

data PhModule a = Module
  { modName :: Maybe (Located Text)
  , modDecls :: [LPhDecl a]
  }

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
                (PhType id)        -- the '[a]' in 'instance Eq [a] where'
                (LPhLocalBinds id) -- The class function implementations
     deriving (Eq, Ord, Show)

-- This will become more interesting when we add records
data ConDecl id
     = ConDecl id [PhType id]
  deriving (Eq, Ord, Show)

instance Outputable b => Outputable (PhModule b) where
    ppr (Module Nothing decls) = vcat (map ppr decls)
    ppr (Module (Just name) decls) =
        vcat $ (text "module" <+> ppr name <+> text "where") : map ppr decls

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

    ppr (ClassDecl scs name tyvar binds) = undefined
    ppr (InstDecl prds name head binds) = undefined

instance Outputable id => Outputable (ConDecl id) where
    ppr (ConDecl name argTypes) = ppr name <+> hcat (map ppr argTypes)
