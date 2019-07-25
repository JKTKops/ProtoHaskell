module Compiler.Parser.Parser
    (
    ) where

import Prelude hiding (lex)

import Compiler.Parser.Lexer
import Compiler.BasicTypes.SrcLoc
import Compiler.BasicTypes.ParsedName
import Compiler.BasicTypes.OccName
import Compiler.BasicTypes.Flags

import Compiler.PhSyn.PhSyn
import Compiler.PhSyn.PhExpr
import Compiler.PhSyn.PhType

import qualified Utils.Outputable as Out

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T

import Data.Maybe (catMaybes, maybe)

import Control.Arrow ((>>>))
import Data.Function ((&))
import Data.Functor  (($>))
import Control.Monad
import Control.Monad.Identity (Identity)
import Control.Monad.Reader

import Text.Parsec hiding (runParser, parse, anyToken, token, satisfy, oneOf, noneOf)
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Error as Parsec
import Text.Parsec.Language (GenLanguageDef(..))
import Text.Parsec.Token    (GenTokenParser)
import qualified Text.Parsec.Token as PT
import qualified Text.Parsec.Prim as Prim (token)
import Text.Parsec.Pos (newPos)

import Debug.Trace

type Parser a = ParsecT
                  [Lexeme]           -- Token stream
                  SrcLoc             -- Location of the end of the previous token
                  (Reader ParseEnv)  -- Reader storing parse environment
                  a

data ParseEnv = ParseEnv
    { indents   :: Column
    , compFlags :: Flags
    }
    deriving (Show)

instance HasCompFlags (ParsecT [Lexeme] SrcLoc (Reader ParseEnv)) where
    getCompFlags = asks compFlags

-----------------------------------------------------------------------------------------
-- Primitive parsers for our Tokens
-----------------------------------------------------------------------------------------

satisfy :: (Token -> Bool) -> Parser Lexeme
satisfy p = try $ optional guardIndentation >> do
    lexeme@(Located pos _) <- Parsec.tokenPrim
                              (unLoc >>> Out.ppr >>> Out.prettyQuote >>> show)
                              posFromTok
                              testTok
    putState (srcSpanEnd pos)
    return lexeme
  where
    posFromTok old _ [] = old
    posFromTok a b (Located _ TokIndent : ls) = posFromTok a b ls
    posFromTok old _ (Located pos _ : _)
      | isGoodSrcSpan pos = let start_pos = srcSpanStart pos
                                file = sourceName old
                                line = unsafeLocLine start_pos
                                col  = unsafeLocCol  start_pos
                                new  = newPos file line col
                            in new
      | otherwise = old
    testTok t = if (p . unLoc) t then Just t else Nothing

guardIndentation :: Parser ()
guardIndentation = do
    i <- getInput
    case i of
        (Located _ TokIndent : ls) -> go
        _ -> fail "" <?> "indentation"
  where
    go = do
        r <- asks indents
        c <- sourceColumn <$> getPosition
        when (c > r)
            (guard False <?> "indentation of " ++ show r ++
                    " (got " ++ show c ++ ")")
        i <- getInput
        setInput $ tail i

token :: Token -> Parser Lexeme
token t = satisfy (== t) <?> (Out.ppr t & Out.prettyQuote & show)

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

backticks :: Parser a -> Parser a
backticks = between (token TokBackquote) (token TokBackquote)

comma :: Parser ()
comma = void $ token TokComma

commaSep :: Parser a -> Parser [a]
commaSep p = sepBy p comma

commaSep1 :: Parser a -> Parser [a]
commaSep1 p = sepBy1 p comma

semicolon :: Parser ()
semicolon = void $ token TokSemicolon

varid :: Parser ParsedName
varid = do
    Located loc tok <- satisfy isVarIdToken
    return $ case tok of
        TokVarId name          -> mkUnQual varName loc name
        TokQualVarId qual name -> mkQual varName loc (qual, name)

varsym :: Parser ParsedName
varsym = do
    Located loc tok <- satisfy isVarSymToken
    return $ case tok of
        TokVarSym name          -> mkUnQual varName loc name
        TokQualVarSym qual name -> mkQual varName loc (qual, name)

tyconid :: Parser ParsedName
tyconid = do
    Located loc tok <- satisfy isConIdToken
    return $ case tok of
        TokConId name          -> mkUnQual tcName loc name
        TokQualConId qual name -> mkQual tcName loc (qual, name)

tyclsid :: Parser ParsedName
tyclsid = tyconid

dataconid :: Parser ParsedName
dataconid = do
    Located loc tok <- satisfy isConIdToken
    return $ case tok of
        TokConId name          -> mkUnQual dataName loc name
        TokQualConId qual name -> mkQual dataName loc (qual, name)

dataconsym :: Parser ParsedName
dataconsym = do
    Located loc tok <- satisfy isConSymToken
    return $ case tok of
        TokConSym name          -> mkUnQual dataName loc name
        TokQualConSym qual name -> mkQual dataName loc (qual, name)

tyvarid :: Parser ParsedName
tyvarid = do
    Located loc tok <- satisfy isVarIdToken
    return $ case tok of
        TokVarId name -> mkUnQual tvName loc name

modlName :: Parser (Located Text)
modlName = do
    Located loc tok <- satisfy isConIdToken
    return . Located loc $ case tok of
        TokConId name -> name
        TokQualConId p1 p2 -> p1 <> "." <> p2

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

-- block, block1 :: Parser a -> Parser [a]
-- block  p = laidout (concat <$> many  (align >> sepEndBy1 p (many1 semicolon)))
-- block1 p = laidout (concat <$> many1 (align >> sepEndBy1 p (many1 semicolon)))

block :: Parser a -> Parser [a]
block p = (catMaybes <$>) $ explicitBlock <|> implicitBlock
  where
    explicitBlock = braces $ optionMaybe p `sepBy` semicolon
    implicitBlock = laidout $ many (Just <$> p)

maybeBraces, maybeBraces1 :: Parser a -> Parser [a]
maybeBraces  p = braces (sepEndBy  p (many semicolon)) <|> block p
maybeBraces1 p = braces (sepEndBy1 p (many semicolon)) <|> block p

locate :: Parser a -> Parser (Located a)
locate p = do
    startPos <- getPosition
    let srcName = sourceName startPos
        startLine = sourceLine startPos
        startCol  = sourceColumn startPos
        startLoc  = mkSrcLoc (T.pack srcName) startLine startCol
    res <- p
    endPos <- getState
    return $ Located (mkSrcSpan startLoc endPos) res

-----------------------------------------------------------------------------------------
-- Main Parser
-----------------------------------------------------------------------------------------

parse :: SourceName -> Flags -> String -> Either String (PhModule ParsedName)
parse srcname flags input = do
    lexemes <- lex srcname input
    case runParser modl srcname flags lexemes of
        Right modl -> Right modl
        Left parseErr -> Left (show parseErr)

runParser :: Parser a -> SourceName -> Flags -> [Lexeme] -> Either ParseError a
runParser p srcname flags lexemes =
    Parsec.runParserT p noSrcLoc srcname lexemes
    & flip runReader (ParseEnv 1 flags)

testParser :: Parser a -> String -> Either String a
testParser p input = do
    lexemes <- lex "" input
    case runParser p "" noFlags lexemes of
        Right v  -> Right v
        Left err -> Left (show err)

modl :: Parser (PhModule ParsedName)
modl = Module <$> optionMaybe modlHeader <*> parseTopDecls

modlHeader :: Parser (Located Text)
modlHeader = do
    reserved "module"
    name <- modlName
    reserved "where"
    return name

parseTopDecls :: Parser [LPhDecl ParsedName]
parseTopDecls = maybeBraces parseTopDecl

parseTopDecl :: Parser (LPhDecl ParsedName)
parseTopDecl = parseDataDecl
           <|> parseSignature
           <|> parseBinding

parseDataDecl :: Parser (LPhDecl ParsedName)
parseDataDecl = locate $ do
    reserved "data"
    typename <- tyconid
    typevars <- many tyvarid
    reservedOp "="
    condecls <- sepBy1 parseConDecl (reservedOp "|")
    return $ DataDecl typename typevars condecls

parseConDecl :: Parser (ConDecl ParsedName)
parseConDecl = do
    name <- dataconid
    fields <- map unLoc <$> many parseType
    return $ ConDecl name fields

parseSignature :: Parser (LPhDecl ParsedName)
parseSignature = locate . fmap Signature
    $ parseFixitySignature <|> try parseTypeSignature

parseTypeSignature :: Parser (Sig ParsedName)
parseTypeSignature = do
    names <- commaSep1 (varid <|> parens varsym)
    reservedOp "::"
    TypeSig names <$> parseType

parseFixitySignature :: Parser (Sig ParsedName)
parseFixitySignature =
    FixitySig <$> parseFixityKeyword
              <*> (maybe 9 fromInteger <$> optionMaybe integer)
              <*> commaSep1 (varsym <|> backticks varid)

parseFixityKeyword :: Parser Assoc
parseFixityKeyword = (reserved "infix"  $> Infix)
                 <|> (reserved "infixl" $> InfixL)
                 <|> (reserved "infixr" $> InfixR)

parseBinding :: Parser (LPhDecl ParsedName)
parseBinding = locate . fmap Binding $ parseBind

parseBind :: Parser (PhBind ParsedName)
parseBind = try parseFunBinding <|> parsePatternBinding

parseFunBinding :: Parser (PhBind ParsedName)
parseFunBinding = do
    funName <- varid
    match   <- locate parseMatch
    return $ FunBind funName $ MG { mgAlts = [match], mgCtxt = FunCtxt }

parsePatternBinding :: Parser (PhBind ParsedName)
parsePatternBinding = PatBind <$> locate parsePattern <*> locate parseRHS

parseMatch :: Parser (Match ParsedName)
parseMatch = do
    pats <- many1 $ locate parsePattern
    rhs  <- locate parseRHS
    mLocalBinds <- optionMaybe $ token TokWhere
    return Match {}

parsePattern = undefined
parseRHS = undefined

-----------------------------------------------------------------------------------------
-- Parsing Types
-----------------------------------------------------------------------------------------

parseType :: Parser (LPhType ParsedName)
parseType = do
    btype <- parseBType
    funtypes <- many (reservedOp "->" >> parseType)
    return $ foldl mkLPhFunTy btype funtypes

parseBType :: Parser (LPhType ParsedName)
parseBType = do
    atypes <- many1 parseAType
    return $ foldl1 mkLPhAppTy atypes

parseAType :: Parser (LPhType ParsedName)
parseAType = locate $
             PhVarTy <$> tyvarid
         <|> (do types <- parens $ commaSep1 parseType
                 case types of
                     -- () is a GTyCon
                     [t] -> return $ PhParTy t
                     ts  -> return $ PhTupleTy ts)
         <|> PhListTy <$> brackets parseType
         <|> parseGTyCon

parseGTyCon :: Parser (PhType ParsedName)
parseGTyCon = PhVarTy <$> tyconid
              -- TODO
              -- Other gtycons are (), [], (->), and (,) (,,) ...
              -- We have to defer these for now until we have the architecture to
              -- wire them in. But we can express them (except ())
              -- via the sugar still.

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
    let Right c = Parsec.parse (PT.charLiteral literalParsers) "" t
    return c

stringLiteral :: Parser String
stringLiteral = do
    Located _ (TokLitString t) <- satisfy
                                (\case TokLitString _ -> True; _ -> False)
    let Right s = Parsec.parse (PT.stringLiteral literalParsers) "" t
    return s


integer :: Parser Integer
integer = do
    Located _ (TokLitInteger t) <- satisfy
                                (\case TokLitInteger _ -> True; _ -> False)
    let Right i = Parsec.parse (PT.integer literalParsers) "" t
    return i


float :: Parser Double
float = do
    Located _ (TokLitFloat t) <- satisfy
                                (\case TokLitFloat _ -> True; _ -> False)
    let Right f = Parsec.parse (PT.float literalParsers) "" t
    return f
