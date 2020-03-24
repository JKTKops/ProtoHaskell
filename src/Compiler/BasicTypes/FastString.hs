{-
Initially I thought we could get by without this module (and we probably could!) but this
module turns out to be important for reducing the headache involved with OccNames.
In particular, OccNames can be messy and very wasteful if they don't share underlying strings,
and they can't be put in UniqueMaps. Additionally, we might end up z-encoding a string
multiple times if it appears in multiple places, but ideally we'd cache those.

We use unsafeDupablePerformIO in order to keep a lookup table around. The lookup table
uniques every FastString and doesn't make a new one if one already exists.

Optimizing this module would probably have a large-scale impact on the compiler performance;
check out what GHC does if you'd like to try.

Implementation ideas are partially taken from GHC, but mostly just the module API.
-}
module Compiler.BasicTypes.FastString where

import Compiler.BasicTypes.Unique
import Utils.Outputable (Outputable(..), text)

import Data.Functor (($>))

-- One initial optimization might be to use 'ByteString' here instead.
-- Doing so might be a bit pervasive, since OccNames are.
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T

import qualified Data.Map as M -- a HashMap over ByteStrings would be the idea for optimization.

import GHC.Exts (IsString(..))
import Data.IORef
import System.IO.Unsafe

data FastString = FS
    { fs_uniq :: {-# UNPACK #-} !Unique
    , fs_text :: !Text
    , fs_zenc :: FastZString -- this field should stay lazy, because not every FastString
                             -- will be Z-Encoded. However, since FastStrings are shared,
                             -- the z-encoding will be computed at most once for any string.
    }

newtype FastZString = FastZString Text
mkFastZString :: String -> FastZString
mkFastZString = FastZString . T.pack

zString :: FastZString -> String
zString = T.unpack . zText

zText :: FastZString -> Text
zText (FastZString text) = text

instance Eq FastString where
    f1 == f2 = fs_uniq f1 == fs_uniq f2

cmpFS :: FastString -> FastString -> Ordering
cmpFS (FS u1 t1 _) (FS u2 t2 _) =
    if u1 == u2 then EQ else compare t1 t2

instance Ord FastString where
    compare = cmpFS

instance IsString FastString where
    fromString = fsLit

instance Semigroup FastString where
    (<>) = appendFS

instance Monoid FastString where
    mempty = nilFS

instance Show FastString where
    show = unpackFS

instance HasUnique FastString where
    getUnique = fs_uniq

instance Outputable FastString where
    ppr = ftext

ftext = text . unpackFS

--------------------------------------------------------------------------------------
--
-- Fast String Table
--
--------------------------------------------------------------------------------------

data FastStringTable = FastStringTable
    -- NOTE: we might need to be more careful about ghc threads here, if multiple
    -- threads try to play with the map at once
    {-# UNPACK #-} !(IORef Int) -- unique counter
    {-# UNPACK #-} !(IORef (M.Map Text FastString))

{-# NOINLINE stringTable #-}
stringTable :: FastStringTable
stringTable = unsafePerformIO $ FastStringTable <$> newIORef 0 <*> newIORef M.empty

mkFastStringWith :: (Unique -> IO FastString) -> Text -> IO FastString
mkFastStringWith mk_fs txt = do
    table <- readIORef tab
    case table M.!? txt of
        Just found -> return found
        Nothing -> do
            u <- getUnique
            new_fs <- mk_fs u
            insert new_fs
  where !(FastStringTable uc tab) = stringTable
        getUnique = atomicModifyIORef' uc $ \n -> (n + 1, Unique PHCFastString n)
        insert fs = modifyIORef tab (M.insert (fs_text fs) fs) $> fs

mkFastString :: String -> FastString
mkFastString str = unsafePerformIO $ mkFastStringWith (pure . mkNewFastString txt) txt
  where txt = T.pack str

mkFastStringText :: Text -> FastString
mkFastStringText txt = unsafePerformIO $ mkFastStringWith (pure . mkNewFastString txt) txt

mkNewFastString :: Text -> Unique -> FastString
mkNewFastString txt u = FS u txt (mkZFastString txt)

-- | Z-Encode a 'Text'.
mkZFastString :: Text -> FastZString
mkZFastString = mkFastZString . error "TODO: zEncodeString"{-zEncodeString-} . T.unpack

--------------------------------------------------------------------------------------
--
-- Utilities
--
--------------------------------------------------------------------------------------

lengthFS :: FastString -> Int
lengthFS = fromIntegral . T.length . fs_text

nullFS :: FastString -> Bool
nullFS = T.null . fs_text

unpackFS :: FastString -> String
unpackFS = T.unpack . fs_text

zEncodeFS :: FastString -> FastZString
zEncodeFS = fs_zenc

appendFS :: FastString -> FastString -> FastString
appendFS fs1 fs2 = mkFastStringText $ T.append (fs_text fs1) (fs_text fs2)

concatFS :: [FastString] -> FastString
concatFS = mkFastStringText . T.concat . map fs_text

headFS :: FastString -> Char
headFS = T.head . fs_text

tailFS :: FastString -> FastString
tailFS = mkFastStringText . T.tail . fs_text

consFS :: Char -> FastString -> FastString
consFS c fs = mkFastStringText $ T.cons c (fs_text fs)

uniqueOfFS :: FastString -> Unique
uniqueOfFS = fs_uniq

nilFS :: FastString
nilFS = fsLit ""

isUnderscoreFS :: FastString -> Bool
isUnderscoreFS = (== fsLit "_")

fsLit = mkFastString
