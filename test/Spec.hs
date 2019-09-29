import Control.Monad
import Test.Tasty

import qualified Compiler.Parser.LexerTest as Lexer

main :: IO ()
main = defaultMain $ testGroup "Tests"
    [ Lexer.tests
    ]
