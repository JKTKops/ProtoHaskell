module Compiler.Parser.Parser
    (
    ) where

import Prelude hiding (lex)

import Compiler.Parser.Lexer
import Compiler.BasicTypes.SrcLoc
import qualified Utils.Outputable as Out

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T

import Control.Arrow ((>>>))
import Control.Monad
import Control.Monad.Identity (Identity)
import Control.Monad.Reader

import Text.Parsec hiding (anyToken, token, satisfy, oneOf, noneOf)
import qualified Text.Parsec as Parsec
import Text.Parsec.Language (GenLanguageDef(..))
import Text.Parsec.Token    (GenTokenParser)
import qualified Text.Parsec.Token as PT
import qualified Text.Parsec.Prim as Prim (token)
import Text.Parsec.Pos (newPos)

type Parser a = ParsecT [Lexeme] () (Reader ParseEnv) a

data ParseEnv = ParseEnv { indents :: Column } deriving (Show)

-----------------------------------------------------------------------------------------
-- Primitive parsers for our Tokens
-----------------------------------------------------------------------------------------

satisfy :: (Token -> Bool) -> Parser Lexeme
satisfy p = do
    pos <- getPosition
    Parsec.tokenPrim (unLoc >>> show) posFromTok testTok
  where
    posFromTok old _ [] = old
    posFromTok old _ (Located pos _ : _)
      | isGoodSrcSpan pos = let start_pos = srcSpanStart pos
                                file = sourceName old
                                line = unsafeLocLine start_pos
                                col  = unsafeLocCol  start_pos
                                new  = newPos file line col
                            in new
      | otherwise = old
    testTok t = if (p . unLoc) t then Just t else Nothing

token :: Token -> Parser Lexeme
token t = satisfy (== t)

oneOf :: [Token] -> Parser Lexeme
oneOf ts = satisfy (`elem` ts)

noneOf :: [Token] -> Parser Lexeme
noneOf ts = satisfy (not . (`elem` ts))

anyToken :: Parser Lexeme
anyToken = satisfy (const True)

reserved :: String -> Parser Lexeme
reserved word = satisfy (== reservedIdToTok word)

reservedOp :: String -> Parser Lexeme
reservedOp op = satisfy (== reservedOpToTok op)

parens :: Parser a -> Parser a
parens = between (token TokLParen) (token TokRParen)

braces :: Parser a -> Parser a
braces = between (token TokLBrace) (token TokRBrace)

brackets :: Parser a -> Parser a
brackets = between (token TokLBracket) (token TokRBracket)

comma :: Parser ()
comma = void $ token TokComma

semicolon :: Parser ()
semicolon = void $ token TokSemicolon

-----------------------------------------------------------------------------------------
-- Implementing Layout Sensitivity
-----------------------------------------------------------------------------------------

laidout :: Parser a -> Parser a
laidout m = do
    cur <- ask
    pos <- sourceColumn <$> getPosition
    local (const (cur { indents = pos })) m

indentCmp :: (Column -> Column -> Bool) -> Parser ()
indentCmp cmp = do
    curPos <- sourceColumn <$> getPosition
    curIndent <- asks indents
    guard $ curPos `cmp` curIndent

indented :: Parser ()
indented = indentCmp (>) <?> "Block (indented)"

align :: Parser ()
align = indentCmp (==) <?> "Block (same indentation)"

explicitSemis :: Parser a -> Parser [a]
explicitSemis p = many semicolon >> (:) <$> p <*> explicitSemis p

block, block1 :: Parser a -> Parser [a]
block  p = laidout (many  $ align >> p)
block1 p = laidout (many1 $ align >> p)

maybeBraces, maybeBraces1 :: Parser a -> Parser [a]
maybeBraces  p = braces (endBy  p semicolon) <|> block p
maybeBraces1 p = braces (endBy1 p semicolon) <|> block p

-----------------------------------------------------------------------------------------
-- Parsers for string literal, char literal, int literal, and float literal values
-----------------------------------------------------------------------------------------

-- We 'cheat' here by using Text.Parsec.Token to generate Haskell Standard
-- compliant parsers for these objects

type LanguageDef = GenLanguageDef Text () Identity
-- Copy of the Text.Parsec.Language.emptyDef, except works with Data.Text
emptyDef :: LanguageDef
emptyDef = PT.LanguageDef
            { PT.commentStart    = ""
            , PT.commentEnd      = ""
            , PT.commentLine     = ""
            , PT.nestedComments  = True
            , PT.identStart      = letter <|> char '_'
            , PT.identLetter     = alphaNum <|> Parsec.oneOf "_'"
            , PT.opStart         = PT.opLetter emptyDef
            , PT.opLetter        = Parsec.oneOf ":!#$%&*+./<=>?@\\^|-~"
            , PT.reservedOpNames = []
            , PT.reservedNames   = []
            , PT.caseSensitive   = True
            }

literalParsers :: GenTokenParser Text () Identity
literalParsers = PT.makeTokenParser emptyDef

-- We can guarantee that the 'parse' call succeeds, because our lexer would
-- not have created a 'TokLitChar' if it was not a syntactically valid char.
charLiteral :: Parser Char
charLiteral = do
    Located _ (TokLitChar t) <- satisfy
                                (\case TokLitChar _ -> True; _ -> False)
    let Right c = parse (PT.charLiteral literalParsers) "" t
    return c

stringLiteral :: Parser String
stringLiteral = do
    Located _ (TokLitString t) <- satisfy
                                (\case TokLitString _ -> True; _ -> False)
    let Right s = parse (PT.stringLiteral literalParsers) "" t
    return s


integer :: Parser Integer
integer = do
    Located _ (TokLitInteger t) <- satisfy
                                (\case TokLitInteger _ -> True; _ -> False)
    let Right i = parse (PT.integer literalParsers) "" t
    return i


float :: Parser Double
float = do
    Located _ (TokLitFloat t) <- satisfy
                                (\case TokLitFloat _ -> True; _ -> False)
    let Right f = parse (PT.float literalParsers) "" t
    return f
