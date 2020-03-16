module Test where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Golden

import System.FilePath
import Data.ByteString.Lazy.UTF8 (fromString)

import Utils.Outputable (Outputable, output)

(==>) :: (Eq a, Show a) => a -> a -> Assertion
(==>) = (@?=)

goldenSimple :: Outputable o
             => TestName
             -> FilePath
             -> (FilePath -> String -> o)
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
