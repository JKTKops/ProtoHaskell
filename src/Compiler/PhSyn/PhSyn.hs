module Compiler.PhSyn.PhSyn where

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T

import Compiler.BasicTypes.SrcLoc (Located)

import Utils.Outputable

data PhModule a = Module
  { modName :: Maybe (Located Text)
  , modDecls :: [LPhDecl a]
  }

instance Outputable b => Outputable (PhModule b) where
    ppr (Module Nothing decls) = vcat (map ppr decls)
    ppr (Module (Just name) decls) =
        vcat $ (text "module" <+> ppr name <+> text "where") : map ppr decls
