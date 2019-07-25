module Compiler.BasicTypes.Unique where

data Unique = Unique !Pass !Int
  deriving (Eq, Show)

data Pass
     = Parse
     | Rename
     | TypeCheck
     | Desugar
     | Simplify
     | TidyCore
     | ToSTG
     | PHCWiredIn
  deriving (Eq, Ord, Show, Enum, Bounded)

mkUniqueSupply :: Pass -> [Unique]
mkUniqueSupply pass = map (Unique pass) [1..]
