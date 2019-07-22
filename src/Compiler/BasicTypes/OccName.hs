module Compiler.BasicTypes.OccName
    ( OccName
    , NameSpace
    , HasOccName(..)

      -- * Constructing OccNames
    , mkOccName
    , mkVarOccName
    , mkDataOccName
    , mkTyVarOccName
    , mkTcOccName
    , mkClsOccName

      -- * Deconstructing OccNames
    , nameSpace
    , nameText
    , occNameSrcSpan

      -- * Constructing NameSpaces
    , tcName, clsName, tcClsName
    , varName, dataName, tvName

      -- * OccName predicates
    , isVarOccName
    , isValOccName
    , isTyVarOccName
    , isDataConOccName
    , isTcClsOccName
    , isSymOccName
    , isConSymOccName

      -- * NameSpace predicates
    , isVarNameSpace
    , isTvNameSpace
    , isDataConNameSpace
    , isValNameSpace
    , isTcClsNameSpace

      -- * NameSpace matching
    , nameSpacesRelated
    , otherNameSpace
    ) where

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Data.Char (isAlphaNum)

import Compiler.BasicTypes.SrcLoc
import Utils.Outputable

{- NOTE: [Names in PHC]

We will have several types of names moving around during compilation.
The 'OccName' type is the most basic.
It is a Text, with a namespace.

-}

-------------------------------------------------------------------------------------
-- The OccName type
-------------------------------------------------------------------------------------

data OccName = OccName
    { nameSpace      :: !NameSpace
    , occNameSrcSpan :: !SrcSpan
    , nameText       :: !Text
    }
    deriving (Eq, Ord, Show)

instance HasOccName OccName where
    occNameOf = id

instance HasSrcSpan OccName where
    srcSpanOf = occNameSrcSpan

instance Outputable OccName where
    ppr (OccName _ _ nt) = ppr nt

mkOccName :: NameSpace -> SrcSpan -> Text -> OccName
mkOccName = OccName

mkVarOccName :: SrcSpan -> Text -> OccName
mkVarOccName = mkOccName varName

mkDataOccName :: SrcSpan -> Text -> OccName
mkDataOccName = mkOccName dataName

mkTyVarOccName :: SrcSpan -> Text -> OccName
mkTyVarOccName = mkOccName tvName

mkTcOccName :: SrcSpan -> Text -> OccName
mkTcOccName = mkOccName tcName

mkClsOccName :: SrcSpan -> Text -> OccName
mkClsOccName = mkOccName clsName

data NameSpace
     = VarName       -- variables
     | DataName      -- data constructors
     | TvName        -- type variables
     | TcClsName     -- Type constructors / type classes
     deriving (Eq, Ord, Show)

-- Now by exporting NameSpace abstract, we can hide the implementation
-- Which makes it easier to change this type in the future if needed.

tcName, clsName, tcClsName :: NameSpace
varName, dataName, tvName  :: NameSpace

tcName    = TcClsName
clsName   = TcClsName
tcClsName = TcClsName

varName  = VarName
dataName = DataName
tvName   = TvName

isVarNameSpace :: NameSpace -> Bool
isVarNameSpace VarName = True
isVarNameSpace TvName  = True
isVarNameSpace _       = False

isTvNameSpace :: NameSpace -> Bool
isTvNameSpace TvName = True
isTvNameSpace _      = False

isDataConNameSpace :: NameSpace -> Bool
isDataConNameSpace DataName = True
isDataConNameSpace _        = False

isValNameSpace :: NameSpace -> Bool
isValNameSpace VarName  = True
isValNameSpace DataName = True
isValNameSpace _        = False

isTcClsNameSpace :: NameSpace -> Bool
isTcClsNameSpace TcClsName = True
isTcClsNameSpace _         = False

pprNameSpace :: NameSpace -> Doc
pprNameSpace VarName   = "variable"
pprNameSpace DataName  = "data constructor"
pprNameSpace TvName    = "type variable"
pprNameSpace TcClsName = "type constructor or class"

-- | Namespaces are related if they can occur in the same contexts
--   This will be useful later for guessing what a user meant
--   when a name is not in scope.
nameSpacesRelated :: NameSpace -> NameSpace -> Bool
nameSpacesRelated ns1 ns2 = ns1 == ns2 || ns1 == otherNameSpace ns2

otherNameSpace :: NameSpace -> NameSpace
otherNameSpace VarName = DataName
otherNameSpace DataName = VarName
otherNameSpace TvName = TcClsName
otherNameSpace TcClsName = TvName

instance Outputable NameSpace where
    ppr = pprNameSpace

-------------------------------------------------------------------------------------
-- OccName predicates
-------------------------------------------------------------------------------------

isVarOccName :: OccName -> Bool
isVarOccName (OccName ns _ _) = isVarNameSpace ns

isTyVarOccName :: OccName -> Bool
isTyVarOccName (OccName ns _ _) = isTvNameSpace ns

isDataConOccName :: OccName -> Bool
isDataConOccName (OccName ns _ _) = isDataConNameSpace ns

isValOccName :: OccName -> Bool
isValOccName (OccName ns _ _) = isValNameSpace ns

isTcClsOccName :: OccName -> Bool
isTcClsOccName (OccName ns _ _) = isTcClsNameSpace ns

isSymOccName :: OccName -> Bool
isSymOccName (OccName _ _ t)
  | isAlphaNum c || c == '_' = False
  | otherwise                = True
  where c = T.head t

isConSymOccName :: OccName -> Bool
isConSymOccName (OccName _ _ t) = T.head t == ':'

-------------------------------------------------------------------------------------
-- HasOccName class
-------------------------------------------------------------------------------------

class HasOccName a where
    occNameOf :: a -> OccName
