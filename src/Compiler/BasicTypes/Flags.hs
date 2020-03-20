{-
Compilation flags. Expect the internals of this module to change somewhat often
as the compiler is developed. It is important that the 'Flags' type remain an
abstract export to facilitate this as its shape will change to include /tons/
of extra information.
-}
module Compiler.BasicTypes.Flags
    ( Flag(..)
    , Flags    -- abstract
    , noFlags
    , fromList
    , member

      -- * Querying Flags
    , wAllOn, wIncompletePatternsOn
    , xMagicHashOn, xTupleSectionsOn
    , dDumpParsedOn, dDumpRenamedOn
    , dDumpTypeCheckOn, dDumpDesugarOn
    , dDumpCoreOn, dDumpSimpleOn
    , dDumpTidyOn, dDumpSTGOn

      -- * Obtaining flags from structures
    , HasCompFlags(..)
    , ContainsCompFlags(..)
    ) where


import Data.Set (Set)
import qualified Data.Set as Set

data Flag
     = WAll
     | WIncompletePatterns

     | XMagicHash
     | XTupleSections

     | DDumpParsed
     | DDumpRenamed
     | DDumpTypecheck
     | DDumpDesguar
     | DDumpCore
     | DDumpSimple
     | DDumpTidy
     | DDumpSTG
     deriving (Eq, Ord, Show, Enum, Bounded)

newtype Flags = Flags { unFlags :: Set Flag }
    deriving (Eq, Ord, Show)

noFlags :: Flags
noFlags = Flags Set.empty

fromList :: [Flag] -> Flags
fromList fs = Flags $ Set.fromList fs

member :: Flag -> Flags -> Bool
member e flags = e `Set.member` unFlags flags

-- could generate these with TH but I don't want to bring TH into this project
wAllOn, wIncompletePatternsOn                   :: Flags -> Bool
xMagicHashOn, xTupleSectionsOn                  :: Flags -> Bool
dDumpParsedOn, dDumpRenamedOn, dDumpTypeCheckOn :: Flags -> Bool
dDumpDesugarOn, dDumpCoreOn, dDumpSimpleOn      :: Flags -> Bool
dDumpTidyOn, dDumpSTGOn                         :: Flags -> Bool

wAllOn                = member WAll
wIncompletePatternsOn = member WIncompletePatterns
xMagicHashOn          = member XMagicHash
xTupleSectionsOn      = member XTupleSections
dDumpParsedOn         = member DDumpParsed
dDumpRenamedOn        = member DDumpRenamed
dDumpTypeCheckOn      = member DDumpTypecheck
dDumpDesugarOn        = member DDumpDesguar
dDumpCoreOn           = member DDumpCore
dDumpSimpleOn         = member DDumpSimple
dDumpTidyOn           = member DDumpTidy
dDumpSTGOn            = member DDumpSTG

-- We intend to make monads instances of this class
class HasCompFlags m where
    getCompFlags :: m Flags

-- And non-monads instances of this class.
class ContainsCompFlags a where
    extractCompFlags :: a -> Flags
