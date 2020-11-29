module Compiler.BasicTypes.Unique where

import Utils.Outputable (Outputable(ppr), text)

data Unique = Unique !UniqueSection !Int
  deriving (Eq, Ord, Show)

-- | Provenance of Uniques.
data UniqueSection
    -- PHC passes
     = ParseSection
     | RenameSection
     | TypeCheckSection
     | DesugarSection
     | SimplifySection
     | TidyCoreSection
     | ToSTGSection

     -- Other reasons we might need a unique:
     | WiredInSection
     | FastStringSection
  deriving (Eq, Ord, Show, Enum, Bounded)

newtype UniqueSupply = UniqueSupply [Unique]

mkUniqueSupply :: UniqueSection -> UniqueSupply
mkUniqueSupply pass = UniqueSupply $ map (Unique pass) [0..]

nextUnique :: UniqueSupply -> (Unique, UniqueSupply)
nextUnique (UniqueSupply (u:us)) = (u, UniqueSupply us)

class HasUnique a where
    getUnique :: a -> Unique

instance HasUnique Unique where
    getUnique = id

instance Outputable UniqueSection where
    ppr ParseSection      = text "p"
    ppr RenameSection     = text "rn"
    ppr TypeCheckSection  = text "tc"
    ppr DesugarSection    = text "ds"
    ppr SimplifySection   = text "simpl"
    ppr TidyCoreSection   = text "tidy"
    ppr ToSTGSection      = text "toSTG"
    ppr WiredInSection    = text "wiredIn"
    ppr FastStringSection = text "fs"

instance Outputable Unique where
    ppr (Unique pass num) = ppr pass <> ppr num
