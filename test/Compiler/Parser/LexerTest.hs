module Compiler.Parser.LexerTest (tests) where
-- test tree matches source tree, which awkwardly puts these outside of the lexer/testcases
-- folder. Consider moving it to test/Compiler/Parser/testcases/lexer/

import Prelude hiding (lex)

import Compiler.BasicTypes.SrcLoc
import Compiler.Parser.Lexer
import Utils.Outputable

import Data.Either

import Test
import Test.Tasty
import Test.Tasty.HUnit

tests :: IO [TestTree]
tests = do
    goldens <- goldenLexerTests
    return [pureTests, goldens]

pureTests :: TestTree
pureTests = testGroup "Lexer Normal Tests"
    [ testIndentationToks
    ]

testIndentationToks :: TestTree
testIndentationToks = testGroup "Indentation Token Insertion" $ map mkLexTest
    [ LexTest
        { testDesc = "No indentation token inserted in empty code"
        , testInput = ""
        , testExpected = Right (==> [])
        }

    , LexTest
        { testDesc = "Indentation token inserted at the start of single-token code"
        , testInput = "module"
        , testExpected = Right $ \r -> do
            head r ==> noLoc TokIndent
            length (filter ((== TokIndent) . unLoc) r) ==> 1
        }

    ]

goldenLexerTests :: IO TestTree
goldenLexerTests = goldenOutput "Lexer Golden Tests"
                                "test/Compiler/Lexer/testcases"
                                lex
                                (const OK)

data LexTest = LexTest
    { testDesc     :: String
    , testInput    :: String
    , testExpected :: Either String ([Lexeme] -> Assertion)
    }

data Result = Success | Failure deriving (Eq, Show)

bool2Result :: Bool -> Result
bool2Result True  = Success
bool2Result False = Failure

mkLexTest :: LexTest -> TestTree
mkLexTest LexTest{testDesc = td, testInput = ti, testExpected = te} =
    testCaseSteps td $ \step -> do
        step "Executing lexer"
        let actual = lex "test.hs" ti

        step "Comparing success/failure"
        bool2Result (isRight actual) @?= bool2Result (isRight te)

        case te of
            Left err -> do
                step "Comparing error messages"
                err @=? fromLeft undefined actual
            Right pred -> do
                step "Verifying result"
                pred $ fromRight undefined actual
