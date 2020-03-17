module Test where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Golden

import System.FilePath
import Data.ByteString.Lazy.UTF8 (fromString)

import Utils.Outputable (Outputable, output)

(==>) :: (Eq a, Show a) => a -> a -> Assertion
(==>) = (@?=)

-- | Simple driver for golden tests. Each test will have the same name as the file
-- that the input comes from.
goldenSimple :: Outputable o
             => TestName -- ^ name of the test tree
             -> FilePath -- ^ directory to search for tests in
             -> (FilePath -> String -> o) -- ^ function to test
             -> IO TestTree
goldenSimple name dir testee = do
    testFiles <- findByExtension [".hs"] dir
    return $ testGroup name
        [ goldenVsString
            (takeBaseName testFile)
            expFile
            (outputOf testFile)
        | testFile <- testFiles
        , let expFile = replaceExtension testFile ".expected"
        ]
  where outputOf tf = do
            inp <- readFile tf
            return $ fromString $ output $ testee tf inp
