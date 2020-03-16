module Compiler.BasicTypes.Unique where

data Unique = Unique !Pass !Int
  deriving (Eq, Show)

data Pass
     = ParsePass
     | RenamePass
     | TypeCheckPass
     | DesugarPass
     | SimplifyPass
     | TidyCorePass
     | ToSTGPass
     | PHCWiredInPass
  deriving (Eq, Ord, Show, Enum, Bounded)

mkUniqueSupply :: Pass -> [Unique]
mkUniqueSupply pass = map (Unique pass) [1..]
