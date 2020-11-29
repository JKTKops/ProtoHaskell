{-
A monad that will be used frequently. Effectively ReaderT env IO.
However, here we assume that computations may fail, so it's more
like MaybeT (ReaderT env IO) but performs much better.

Adapated from GHC.
-}
module Control.Monad.IOEnv
    ( IOEnv
    , runIOEnv, ioEnvFail

    -- * Mutable variables in IOEnv
    , MutVar, newMutVar, readMutVar, writeMutVar
    , updMutVar, atomicUpdMutVar, atomicUpdMutVar'

    -- * Accessing the environment
    , getEnv, inEnv, updEnv
    ) where

import Compiler.Settings

import System.IO (fixIO)
import System.IO.Unsafe (unsafeInterleaveIO)

import Control.Applicative (Alternative(..))
import Control.Monad (MonadPlus(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Fix (MonadFix(..))
import Control.Exception (catch, throwIO, Exception)

import Data.IORef

-- | Basically ReaderT env IO.
newtype IOEnv env a = IOEnv { unIOEnv :: env -> IO a }

-- We could use deriving via (probably should in the future)
-- but this is more fun!
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
instance MonadFail (IOEnv env) where
    fail _ = ioEnvFail

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

runIOEnv :: env -> IOEnv env a -> IO (Maybe a)
runIOEnv env ioEnv = unIOEnv (ioEnvCatch ioEnv) env

fixM :: (a -> IOEnv env a) -> IOEnv env a
fixM f = IOEnv $ \env -> fixIO $ \r -> unIOEnv (f r) env
{-# NOINLINE fixM #-} -- according to GHC, this fixes a space leak

ioEnvFail :: IOEnv env a
ioEnvFail = IOEnv $ \_ -> throwIO IOEnvFailure

ioEnvCatch :: IOEnv env a -> IOEnv env (Maybe a)
ioEnvCatch (IOEnv f) = IOEnv $ \env ->
    catch (Just <$> f env) $ \IOEnvFailure ->
        pure Nothing

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
