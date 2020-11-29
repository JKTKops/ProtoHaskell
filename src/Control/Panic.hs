module Control.Panic where

import Control.Exception
import System.Environment (getProgName)
import System.IO.Unsafe (unsafePerformIO)

import Paths_ProtoHaskell (version)

-- | These are exceptions that we will never catch. If one of these happens,
-- the compiler is crashing.
data PhcException
     = Panic String
     | Sorry String

instance Exception PhcException
instance Show PhcException where
    show e = progName ++ ": " ++ showPhcException e

showPhcException :: PhcException -> String
showPhcException = \case
    Panic msg -> panicMsg msg
    Sorry msg -> sorryMsg msg
  where
    panicMsg :: String -> String
    panicMsg s = "panic! The \"impossible\" has happened.\n"
              ++ "  (PHC version: " ++ show version ++ "):\n"
              ++ "    " ++ s ++ "\n\n"
              ++ "Please report this as a bug."
    sorryMsg :: String -> String
    sorryMsg s = "sorry! You've run into an unimplemented feature or known bug.\n"
              ++ "  (PHC version: " ++ show version ++ "):\n"
              ++ "    " ++ s

panic, sorry :: String -> a
panic = throw . Panic
sorry = throw . Sorry

-- | The name of this instance of PHC.
progName :: String
progName = unsafePerformIO getProgName
{-# NOINLINE progName #-}

shortUsage :: String
shortUsage = "Usage: for basic information, try --help."
