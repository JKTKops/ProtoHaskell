module Compiler.BasicTypes.ParsedName where

import Data.Text.Lazy (Text)

import Compiler.BasicTypes.OccName

data ParsedName
     = UnQual OccName
       -- ^ An unqualified name directly from the source.

     | Qual Text OccName
       -- ^ A qualified name directly from the source.
       -- the module name is the name of the module from which it is imported
       -- not necessarily the module in which it is defined.

     | Orig Text OccName
       -- ^ 'Original' name. Module name is the defining module.
       -- We use these when we generate code and want to force the use
       -- of a specific function, eg, 'Prelude.map'.

     | Exact Name
       -- ^ An exact 'name'. Used when parsing syntax like '[]'.
       -- Can only be created by 'Name#getParsedName'.

instance HasOccName ParsedName where
    occNameOf = parsedNameOcc

parsedNameOcc :: ParsedName -> OccName
parsedNameOcc (UnQual n) = n
parsedNameOcc (Qual _ n) = n
parsedNameOcc (Orig _ n) = n
parsedNameOcc (Exact  n) = occNameOfName n

parsedNameSpace :: ParsedName -> NameSpace
parsedNameSpace = nameSpace . parsedNameOcc
