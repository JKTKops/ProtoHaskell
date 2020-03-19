module Test
    ( (==>)
    , golden
    , goldenOutput
    , goldenShow
    , goldenPShow
    , CheckOutput(..)
    ) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Golden
import Test.Tasty.Golden.Advanced

import System.FilePath
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy.Encoding as T
import Data.ByteString.Lazy.UTF8 (ByteString, fromString)
import qualified Data.ByteString.Lazy as LBS (readFile, writeFile, unpack)

import Text.Pretty.Simple
import Utils.Outputable (Outputable, output)

(==>) :: (Eq a, Show a) => a -> a -> Assertion
(==>) = (@?=)

class ByteStringable s where
    toByteString :: s -> ByteString

instance ByteStringable String where
    toByteString = fromString
instance ByteStringable T.Text where
    toByteString = T.encodeUtf8

data CheckOutput = OK | FailFast String

-- | Test output will use @output@ from Utils.Outputable.
goldenOutput name dir testee check = golden name dir testee check output

-- | Test output uses pShowNoColor from pretty-simple.
-- use this for tests that should be human readable, which is probably all of them.
goldenPShow name dir testee check = golden name dir testee check pShowNoColorIndent2

-- | Test output uses show.
-- Try to avoid using this one.
goldenShow name dir testee check = golden name dir testee check show

pShowNoColorIndent2 :: Show a => a -> T.Text
pShowNoColorIndent2 = pShowOpt defaultOutputOptionsDarkBg
                                { outputOptionsIndentAmount = 2
                                , outputOptionsColorOptions = Nothing
                                }

-- | Slightly more general driver for golden tests. Also takes a function to convert
-- the output of the test to something that can be serialized to a golden file
-- (typically String or Text).
golden :: ByteStringable s
       => TestName -- ^ name of the test tree
       -> FilePath -- ^ directory to search for tests in
       -> (FilePath -> String -> a) -- ^ function to test
       -> (a -> CheckOutput) -- ^ check if the test should fail fast
       -> (a -> s) -- ^ function to convert output
       -> IO TestTree
golden name dir testee check display = do
    testFiles <- findByExtension [".hs"] dir
    return $ testGroup name
        [ goldenTest
            (takeBaseName testFile)
            (expFrom expFile)
            (test testFile)
            (compare expFile)
            (upd expFile)
        | testFile <- testFiles
        , let expFile = replaceExtension testFile ".expected"
        ]
  where test tf = do
            inp <- readFile tf
            let r = testee tf inp
            return $ case check r of
                OK           -> Right $ toByteString $ display r
                FailFast msg -> Left msg

        expFrom expFile = Right <$> LBS.readFile expFile

        compare _ (Left msg) _ = pure $ Just msg
        compare _ _ (Left msg) = pure $ Just msg
        compare fn (Right bs1) (Right bs2) =
            if bs1 == bs2
            then pure Nothing
            else do
                LBS.writeFile (replaceExtension fn ".actual") bs2
                return $ Just "Output was incorrect."

        upd fn (Right bs) = LBS.writeFile fn bs
