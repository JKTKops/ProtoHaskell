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

      -- * Obtaining flags inside a Monad
    , HasCompFlags(..)
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
