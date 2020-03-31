{-
Compilation flags. Expect the internals of this module to change somewhat often
as the compiler is developed. It is important that the 'Settings' type remain an
abstract export to facilitate this as its shape will change to include /tons/
of extra information.
-}
module Compiler.Settings
    ( WarnFlag(..)
    , LangFlag(..)
    , DumpFlag(..)
    , Settings    -- abstract
    , defaultSettings

      -- * Querying Flags
    , warnOpt, langOpt, dumpOpt

      -- * Obtaining flags from structures
    , HasSettings(..)
    , ContainsSettings(..)
    ) where

-- NOTE: perhaps we should be using 'EnumSet's?
import Data.Set (Set)
import qualified Data.Set as Set

-- | Flags that start with -W
data WarnFlag
     = WAll
     | WIncompletePatterns
     deriving (Eq, Ord, Show, Enum, Bounded)

-- | Flags that start with -X
data LangFlag
     = XMagicHash
     | XTupleSections
     deriving (Eq, Ord, Show, Enum, Bounded)

-- | Flags that start with -ddump
-- Other flags can be twiddled by throwing "no" after their prefix,
-- e.x. -Wno-incomplete-patterns.
-- But these can't.
data DumpFlag
     = DDumpParsed
     | DDumpRenamed
     | DDumpTypecheck
     | DDumpDesguar
     | DDumpCore
     | DDumpSimple
     | DDumpTidy
     | DDumpSTG
     deriving (Eq, Ord, Show, Enum, Bounded)

-- | Flags that (typically) start with -f.
-- Some flags that you might expect to be here,
-- instead have their
-- own field in the 'Settings' record, because
-- "unspecified" may mean Auto, rather than Never.
data GeneralFlag = GeneralFlag -- Don't have any of these yet. These are for cool features
                               -- after the compiler is done!
     deriving (Eq, Ord, Show, Enum, Bounded)

data Settings = Settings
    { warningFlags  :: Set WarnFlag
    , languageFlags :: Set LangFlag
    , dumpFlags     :: Set DumpFlag
    , generalFlags  :: Set GeneralFlag

    , canUseColor   :: Bool
    , useColor      :: OverridingBool
    }
    deriving (Eq, Ord, Show)

defaultSettings :: Settings
defaultSettings = Settings
    { warningFlags  = Set.empty
    , languageFlags = Set.empty
    , dumpFlags     = Set.empty
    , generalFlags  = Set.empty

    -- needs to be initialized in IO by checking if stderr supports colors
    , canUseColor   = False
    , useColor      = Auto
    }

warnOpt :: WarnFlag -> Settings -> Bool
warnOpt wf s = wf `Set.member` warningFlags s

langOpt :: LangFlag -> Settings -> Bool
langOpt xf s = xf `Set.member` languageFlags s

dumpOpt :: DumpFlag -> Settings -> Bool
dumpOpt df s = df `Set.member` dumpFlags s

gOpt :: GeneralFlag -> Settings -> Bool
gOpt gf s = gf `Set.member` generalFlags s

shouldUseColor :: Settings -> Bool
shouldUseColor stngs = canUseColor stngs `overrideWith` useColor stngs

-- We intend to make monads instances of this class
class HasSettings m where
    getSettings :: m Settings

-- And non-monads instances of this class.
class ContainsSettings a where
    extractSettings :: a -> Settings

data OverridingBool = Auto | Always | Never
  deriving (Eq, Ord, Show, Enum, Bounded)

overrideWith :: Bool -> OverridingBool -> Bool
overrideWith b Auto   = b
overrideWith _ Always = True
overrideWith _ Never  = False
infixr 4 `overrideWith` -- binds tighter than && and ||

-- | Reasons associate with messages containing warnings.
-- The message may be something that we always print,
-- it may have been enabled by a flag, or it may have been turned into an
-- error by a flag.
data WarnReason
     = NoReason
     | WarnReason WarnFlag
     | ErrReason  WarnFlag
