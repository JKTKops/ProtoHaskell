module Compiler.Parser.ParserTest (tests) where

import Prelude hiding (lex)

import Compiler.Settings
import Compiler.Parser.Parser

import Test
import Test.Tasty

tests :: IO [TestTree]
tests = do
    goldenTests <- sequence [shouldSucceed, shouldFail]
    return [testGroup "Parser Golden Tests" goldenTests]

shouldSucceed :: IO TestTree
shouldSucceed = goldenPShow "Should Succeed"
                            "test/Compiler/Parser/testcases/shouldsucceed"
                            runParse
                            (\case
                                    Left _ -> FailFast "parse failed"
                                    Right _ -> OK)

-- pshow behaves strangely since CDoc's show instance is not derived
shouldFail :: IO TestTree
shouldFail = goldenOutput "Should Fail"
                          "test/Compiler/Parser/testcases/shouldfail"
                          runParse
                          (\case
                                  Left _  -> OK
                                  Right _ -> FailFast "parse succeeded")

runParse fn = parse fn defaultSettings
