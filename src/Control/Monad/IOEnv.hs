{-
A monad that will be used frequently. Effectively ReaderT env IO.

Adapated from GHC.
-}
module Control.Monad.IOEnv where

import Compiler.Settings

import System.IO (fixIO)
import System.IO.Unsafe (unsafeInterleaveIO)

import Control.Applicative (Alternative(..))
import Control.Monad (MonadPlus(..))
import Control.Monad.IO.Class
import Control.Monad.Fail
import Control.Monad.Fix
import Control.Exception

import Data.IORef

-- | Basically ReaderT env IO, custom-rolled so we can make our own instances.
-- Could use deriving-via for the ones that are the same, but keeping it simple.
newtype IOEnv env a = IOEnv { unIOEnv :: env -> IO a }

instance Functor (IOEnv env) where
    fmap f (IOEnv r) = IOEnv $ \env -> fmap f (r env)

instance Applicative (IOEnv env) where
    pure = pureM
    IOEnv f <*> IOEnv x = IOEnv (\env -> f env <*> x env)
    (*>) = thenM_

instance Monad (IOEnv env) where
    (>>=) = thenM

instance Alternative (IOEnv env) where
    empty = IOEnv $ const empty
    IOEnv m <|> IOEnv k = IOEnv $ \env -> m env <|> k env
instance MonadPlus (IOEnv env)

instance MonadIO (IOEnv env) where
    liftIO io = IOEnv $ const io
instance MonadFix (IOEnv env) where
    mfix = fixM

pureM :: a -> IOEnv env a
pureM a = IOEnv $ \_ -> pure a

thenM :: IOEnv env a -> (a -> IOEnv env b) -> IOEnv env b
thenM (IOEnv m) f = IOEnv $ \env -> do r <- m env; unIOEnv (f r) env

thenM_ :: IOEnv env a -> IOEnv env b -> IOEnv env b
thenM_ m k = IOEnv $ \env -> do unIOEnv m env; unIOEnv k env

data IOEnvFailure = IOEnvFailure
instance Show IOEnvFailure where show _ = "IOEnv failure"
instance Exception IOEnvFailure

instance ContainsSettings env => HasSettings (IOEnv env) where
    getSettings = do env <- getEnv
                      -- flags won't change often (during compilation, only via options pragmas)
                     return $! extractSettings env

--------------------------------------------------------------------------------------
--
-- Combinators
--
--------------------------------------------------------------------------------------

runIOEnv :: env -> IOEnv env a -> IO a
runIOEnv = flip unIOEnv

fixM :: (a -> IOEnv env a) -> IOEnv env a
fixM f = IOEnv $ \env -> fixIO $ \r -> unIOEnv (f r) env
{-# NOINLINE fixM #-} -- according to GHC, this fixes a space leak

tryM :: IOEnv env r -> IOEnv env (Either IOEnvFailure r)
tryM (IOEnv f) = IOEnv $ tryIOEnvFailure . f

tryIOEnvFailure :: IO r -> IO (Either IOEnvFailure r)
tryIOEnvFailure = try

unsafeInterleaveM :: IOEnv env a -> IOEnv env a
unsafeInterleaveM (IOEnv m) = IOEnv $ \env -> unsafeInterleaveIO (m env)

--------------------------------------------------------------------------------------
--
-- Mutable Variables
--
--------------------------------------------------------------------------------------

type MutVar a = IORef a
newMutVar :: a -> IOEnv env (MutVar a)
newMutVar = liftIO . newIORef

writeMutVar :: MutVar a -> a -> IOEnv env ()
writeMutVar var val = liftIO (writeIORef var val)

readMutVar :: MutVar a -> IOEnv env a
readMutVar = liftIO . readIORef

updMutVar :: MutVar a -> (a -> a) -> IOEnv env ()
updMutVar var upd = liftIO $ modifyIORef var upd

-- | Non-strict atomic update.
atomicUpdMutVar :: MutVar a -> (a -> (a, b)) -> IOEnv env b
atomicUpdMutVar var upd = liftIO $ atomicModifyIORef var upd

-- | Strict atomic update.
atomicUpdMutVar' :: MutVar a -> (a -> (a, b)) -> IOEnv env b
atomicUpdMutVar' var upd = liftIO $ atomicModifyIORef' var upd

--------------------------------------------------------------------------------------
--
-- Accessing the environment
--
--------------------------------------------------------------------------------------

getEnv :: IOEnv env env
getEnv = IOEnv pure
{-# INLINE getEnv #-}

inEnv :: env0 -> IOEnv env0 a -> IOEnv env a
inEnv env = updEnv (const env)
{-# INLINE inEnv #-}

updEnv :: (env0 -> env1) -> IOEnv env1 a -> IOEnv env0 a
updEnv f (IOEnv m) = IOEnv $ \env -> m (f env)
