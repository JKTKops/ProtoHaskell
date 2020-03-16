import Control.Monad
import Test.Tasty

import qualified Compiler.Parser.LexerTest as Lexer

tests :: IO TestTree
tests = do
    lexerTests <- Lexer.tests

    return $ testGroup "Tests"
      [ lexerTests ]

main :: IO ()
main = tests >>= defaultMain
