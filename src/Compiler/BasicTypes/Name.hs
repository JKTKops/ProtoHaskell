module Compiler.BasicTypes.Name
    ( Name
    , NameSort(..)
    , nameUnique, nameOccName
    , nameSrcLoc, nameSrcSpan

    , isTyVarName, isTyConName
    , isDataConName
    , isValName, isVarName
    , isSystemName
    ) where


import Compiler.BasicTypes.OccName
import Compiler.BasicTypes.Unique
import Compiler.BasicTypes.SrcLoc

import Utils.Outputable

data Name = Name
    { n_sort :: NameSort
    , n_occ  :: !OccName
    , n_uniq :: !Unique -- N.B. this unique disambiguates OccName's with the same unique
                        -- because all OccNames with the same text share a unique.
    }
  deriving Show

data NameSort
     = UserDefined -- internal to module being compiled
                   -- will add External in the future
     | WiredIn
     | System
  deriving (Eq, Ord, Show, Enum, Bounded)

instance Outputable NameSort where
    ppr UserDefined = text "user-defined"
    ppr WiredIn = text "wired-in"
    ppr System  = text "system"

instance HasSrcSpan Name where
    srcSpanOf = srcSpanOf . nameOccName

instance HasOccName Name where
    occNameOf = nameOccName

instance ContainsUnique Name where
    getUnique = n_uniq

nameUnique  :: Name -> Unique
nameOccName :: Name -> OccName
nameSrcLoc  :: Name -> SrcLoc
nameSrcSpan :: Name -> SrcSpan

nameUnique  = n_uniq
nameOccName = n_occ
nameSrcLoc  = srcSpanStart . occNameSrcSpan . n_occ
nameSrcSpan = occNameSrcSpan . n_occ

isTyVarName :: Name -> Bool
isTyVarName = isTyVarOccName . nameOccName

isTyConName :: Name -> Bool
isTyConName = isTcClsOccName . nameOccName

isDataConName :: Name -> Bool
isDataConName = isDataConOccName . nameOccName

isValName :: Name -> Bool
isValName = isValOccName . nameOccName

isVarName :: Name -> Bool
isVarName = isVarOccName . nameOccName

isSystemName :: Name -> Bool
isSystemName Name{n_sort = System} = True
isSystemName _ = False


instance Outputable Name where
    ppr Name{n_occ = name} = ppr name
