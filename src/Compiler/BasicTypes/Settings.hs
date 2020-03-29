{-
Compilation flags. Expect the internals of this module to change somewhat often
as the compiler is developed. It is important that the 'Settings' type remain an
abstract export to facilitate this as its shape will change to include /tons/
of extra information.
-}
module Compiler.BasicTypes.Settings
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


import Data.Set (Set)
import qualified Data.Set as Set

data WarnFlag
     = WAll
     | WIncompletePatterns
     deriving (Eq, Ord, Show, Enum, Bounded)

data LangFlag
     = XMagicHash
     | XTupleSections
     deriving (Eq, Ord, Show, Enum, Bounded)

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

data Settings = Settings
    { warningFlags  :: Set WarnFlag
    , languageFlags :: Set LangFlag
    , dumpFlags     :: Set DumpFlag
    }
    deriving (Eq, Ord, Show)

defaultSettings :: Settings
defaultSettings = Settings
    { warningFlags  = Set.empty
    , languageFlags = Set.empty
    , dumpFlags     = Set.empty
    }

warnOpt :: WarnFlag -> Settings -> Bool
warnOpt wf s = wf `Set.member` warningFlags s

langOpt :: LangFlag -> Settings -> Bool
langOpt xf s = xf `Set.member` languageFlags s

dumpOpt :: DumpFlag -> Settings -> Bool
dumpOpt df s = df `Set.member` dumpFlags s

-- We intend to make monads instances of this class
class HasSettings m where
    getSettings :: m Settings

-- And non-monads instances of this class.
class ContainsSettings a where
    extractSettings :: a -> Settings
