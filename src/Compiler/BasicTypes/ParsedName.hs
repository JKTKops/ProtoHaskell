module Compiler.BasicTypes.ParsedName
    ( ParsedName
    , mkUnQual
    , mkQual
    , mkOrig
    , parsedNameOcc
    , parsedNameSpace
    , getParsedName
    , isSrcParsedName
    ) where

import Data.Text.Lazy (Text)

import Compiler.BasicTypes.SrcLoc
import Compiler.BasicTypes.OccName
import Compiler.BasicTypes.Name

import Utils.Outputable

data ParsedName
     = UnQual OccName
       -- ^ An unqualified name directly from the source.

     | Qual Text OccName
       -- ^ A qualified name directly from the source.
       -- the module name is the (possibly qualified) name of the module from which
       -- it is imported; not necessarily the module in which it is defined.

     | Orig Text OccName
       -- ^ 'Original' name. Module name is the defining module.
       -- We use these when we generate code and want to force the use
       -- of a specific function, eg, 'Prelude.map'.

     | Exact Name
       -- ^ An exact 'Name'. Used when parsing syntax like '[]'.
       -- Can only be created by 'getParsedName'.
  deriving Show

mkUnQual :: NameSpace -> SrcSpan -> Text -> ParsedName
mkUnQual ns ss t = UnQual (mkOccName ns ss t)

mkQual :: NameSpace -> SrcSpan -> (Text, Text) -> ParsedName
mkQual ns ss (qual, t) = Qual qual (mkOccName ns ss t)

mkOrig :: Text -> OccName -> ParsedName
mkOrig = Orig

instance HasOccName ParsedName where
    occNameOf = parsedNameOcc

instance HasSrcSpan ParsedName where
    srcSpanOf = srcSpanOf . occNameOf

parsedNameOcc :: ParsedName -> OccName
parsedNameOcc (UnQual n) = n
parsedNameOcc (Qual _ n) = n
parsedNameOcc (Orig _ n) = n
parsedNameOcc (Exact  n) = nameOccName n

parsedNameSpace :: ParsedName -> NameSpace
parsedNameSpace = nameSpace . parsedNameOcc

getParsedName :: Name -> ParsedName
getParsedName = Exact

isSrcParsedName :: ParsedName -> Bool
isSrcParsedName (UnQual _) = True
isSrcParsedName (Qual _ _) = True
isSrcParsedName _ = False

instance Outputable ParsedName where
    ppr (UnQual n) = ppr n
    ppr (Qual qual n) = ppr qual <> char '.' <> ppr n
    ppr (Orig qual n) = ppr qual <> char '.' <> ppr n
    ppr (Exact name)  = ppr name
