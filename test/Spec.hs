import Test.Tasty

import qualified Compiler.Parser.LexerTest as Lexer
import qualified Compiler.Parser.ParserTest as Parser

tests :: IO TestTree
tests = do
    lexerTests <- Lexer.tests
    parserTests <- Parser.tests

    return $ testGroup "Tests" $
         lexerTests
      <> parserTests

main :: IO ()
main = tests >>= defaultMain
