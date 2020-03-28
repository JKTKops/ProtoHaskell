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
    , nameFS
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
import Compiler.BasicTypes.Unique
import Compiler.BasicTypes.FastString
import Utils.Outputable

{- NOTE: [Names in PHC]

We will have several types of names moving around during compilation.
The 'OccName' type is the most basic.
It is a FastString, with a namespace and a source location.

-}

-------------------------------------------------------------------------------------
-- The OccName type
-------------------------------------------------------------------------------------

data OccName = OccName
    { nameSpace      :: !NameSpace
    , occNameSrcSpan :: !SrcSpan
    , nameFS         :: !FastString
    }
    deriving (Eq, Ord, Show)

nameText :: OccName -> Text
nameText = fs_text . nameFS

instance HasOccName OccName where
    occNameOf = id

instance HasSrcSpan OccName where
    srcSpanOf = occNameSrcSpan

instance HasUnique OccName where
    getUnique = getUnique . nameFS

-- N.B. while it is possible to define HasOccName a => HasUnique a, we should NOT do this.
-- occname uniques are from the fast string, and don't actually uniquely identify whatever
-- entity might /contain/ the OccName. Entities with OccNames typically have a Name, and the
-- unique of the Name typically uniquely identifies the entity as well.

instance Outputable OccName where
    ppr (OccName _ _ fs) = text $ show fs

mkOccName :: NameSpace -> SrcSpan -> Text -> OccName
mkOccName ns span txt = OccName ns span (mkFastStringText txt)

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
isVarNameSpace ns = ns `elem` [VarName, TvName]

isTvNameSpace :: NameSpace -> Bool
isTvNameSpace = (== TvName)

isDataConNameSpace :: NameSpace -> Bool
isDataConNameSpace = (== DataName)

isValNameSpace :: NameSpace -> Bool
isValNameSpace ns = ns `elem` [VarName, DataName]

isTcClsNameSpace :: NameSpace -> Bool
isTcClsNameSpace = (== TcClsName)

pprNameSpace :: NameSpace -> CDoc
pprNameSpace VarName   = text "variable"
pprNameSpace DataName  = text "data constructor"
pprNameSpace TvName    = text "type variable"
pprNameSpace TcClsName = text "type constructor or class"

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
isSymOccName (OccName _ _ fs)
  | isAlphaNum c || c == '_' = False
  | otherwise                = True
  where c = headFS fs

isConSymOccName :: OccName -> Bool
isConSymOccName (OccName _ _ fs) = headFS fs == ':'

-------------------------------------------------------------------------------------
-- HasOccName class
-------------------------------------------------------------------------------------

class HasOccName a where
    occNameOf :: a -> OccName
