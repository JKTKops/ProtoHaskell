module Compiler.Parser.Helpers
    ( module Compiler.Parser.Helpers
    , module Compiler.Parser.Lexer

    , module Compiler.BasicTypes.SrcLoc
    , module Compiler.BasicTypes.ParsedName
    , module Compiler.BasicTypes.OccName
    , module Compiler.BasicTypes.Settings

    , module Text.Parsec
    , module Control.Monad
    , module Data.Functor -- for $>
    )where

import Prelude hiding (lex)

import Compiler.Parser.Lexer
import Compiler.Parser.Errors

import Compiler.BasicTypes.SrcLoc
import Compiler.BasicTypes.ParsedName
import Compiler.BasicTypes.OccName
import Compiler.BasicTypes.Settings

import qualified Utils.Outputable as Out

import Data.Maybe (catMaybes, isJust)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T

import Data.Function ((&))
import Data.Functor  (($>))
import Control.Arrow ((>>>))
import Control.Monad
import Control.Monad.Identity

import Text.Parsec hiding ( anyToken, label, (<?>), noneOf
                          , oneOf, runParser, satisfy, token
                          , parse)
import qualified Text.Parsec as Parsec
import Text.Parsec.Pos (newPos)

import Text.Parsec.Language (GenLanguageDef(..))
import Text.Parsec.Token    (GenTokenParser)
import qualified Text.Parsec.Token as PT


type Parser a = Parsec
                  [Lexeme]           -- Token stream
                  ParseState
                  a

data ParseState = ParseState
    { compFlags      :: Settings
    , indentOrd      :: IndentOrdering
    , layoutContexts :: [LayoutContext]
    , endOfPrevToken :: SrcLoc
    }
    deriving (Show)

data IndentOrdering = Eq | Gt | Geq deriving (Eq, Ord, Show, Read, Enum, Bounded)

data LayoutContext
     = Explicit
     | Implicit Int
     deriving (Eq, Ord, Show)

initParseState flags = ParseState flags Eq [] noSrcLoc

pushLayoutContext :: LayoutContext -> Parser ()
pushLayoutContext ctx = modifyState $ \s@ParseState{ layoutContexts } ->
    s { layoutContexts = ctx : layoutContexts }

popLayoutContext :: Parser ()
popLayoutContext = do
    ctx <- gets layoutContexts
    case ctx of
        [] -> fail "Tried to pop a layout context, but there are no layout contexts"
        (_:ctxs) -> modify $ \s -> s { layoutContexts = ctxs }

get :: Parser ParseState
get = getState

gets :: (ParseState -> a) -> Parser a
gets f = f <$> get

put :: ParseState -> Parser ()
put = putState

modify :: (ParseState -> ParseState) -> Parser ()
modify = modifyState

currentLayoutContext :: Parser (Maybe LayoutContext)
currentLayoutContext = do
    ctxs <- gets layoutContexts
    return $ case ctxs of
        [] -> Nothing
        (x:_) -> Just x

label :: Parser a -> String -> Parser a
label p exp = do
    mctx <- currentLayoutContext
    case mctx of
        Nothing -> Parsec.label p exp
        Just Explicit -> Parsec.label p exp
        Just (Implicit n) -> labelWithIndentInfo p exp n
  where
    labelWithIndentInfo p exp n = do
        ord <- gets indentOrd
        let ordPiece = case ord of
                Eq  -> show n
                Gt  -> "greater than " ++ show n
                Geq -> "less than" ++ show n -- Shouldn't happen
            indentPiece = "at indentation"
        Parsec.label p $ unwords [exp, indentPiece, ordPiece]

(<?>) = label
infixl 0 <?> -- I disagree with this fixity but it's what Parsec uses

-- | Anticipate a user error, producing an error like:
-- "unexpected <token>, perhaps you meant <msg>"
anticipate :: Token -> String -> Parser a
anticipate t msg = do
    lookAhead $ token t
    unexpected $ showTokenPretty t ++ ", perhaps you meant " ++ msg ++ "?"

anticipateOp :: String -> String -> Parser a
anticipateOp op msg = anticipate (reservedOpToTok op) msg

instance HasSettings (Parsec [Lexeme] ParseState) where
    getSettings = compFlags <$> getState

{- NOTE: [Overlapping Show instance for Lexeme]

For some reason, some parsec combinators seem to enforce a Show instance on Lexeme
which they then use inside `unexepctected` messages. This is catastrophic!
Bonus points for switching to `Megaparsec` if version 8 can handle custom streams properly.

To remedy this, we have to overlap the existing (informative) show instance for Lexeme
in order to be able to guarantee that the user sees pretty-printed error messages.

-}
instance {-# OVERLAPPING #-} Show (GenLocated SrcSpan Token) where
    show (Located _ TokIndent) = "indentation"
    show (Located _ t)         = showTokenPretty t

-----------------------------------------------------------------------------------------
-- Primitive parsers for our Tokens
-----------------------------------------------------------------------------------------

satisfy :: (Token -> Bool) -> Parser Lexeme
satisfy p = try $ guardIndentation *> satisfyNoIndentGuard p <* setIndentOrdGT
  where setIndentOrdGT = modify $ \s -> s { indentOrd = Gt }

-- | This parser is unsafe, as it breaks the guarantees we make about
-- checking indentation. However it is more efficient when it is known
-- that an indentation check should not happen. Use with caution!
satisfyNoIndentGuard :: (Token -> Bool) -> Parser Lexeme
satisfyNoIndentGuard p = do
    lexeme@(Located pos _) <- Parsec.tokenPrim
                              (unLoc >>> showTokenPretty)
                              posFromTok
                              testTok
    modifyState $ \s -> s { endOfPrevToken = srcSpanEnd pos }
    return lexeme
  where
    testTok t = if (p . unLoc) t then Just t else Nothing

posFromTok :: SourcePos -> t -> [Lexeme] -> SourcePos
posFromTok old _ [] = old
posFromTok a b (Located _ TokIndent : ls) = posFromTok a b ls
posFromTok old _ (Located pos _ : _)
  | isGoodSrcSpan pos = mkSrcPos $ srcSpanStart pos
  | otherwise = old

mkSrcPos :: SrcLoc -> SourcePos
mkSrcPos loc = let file = unsafeLocFile loc
                   line = unsafeLocLine loc
                   col  = unsafeLocCol  loc
                   new  = newPos (T.unpack file) line col
               in new

guardIndentation :: Parser ()
guardIndentation = do
    check <- optionMaybe $ satisfyNoIndentGuard (== TokIndent)
    ord <- gets indentOrd
    when (isJust check || ord == Eq) $ do
        mr <- currentLayoutContext
        case mr of
            Nothing -> return ()
            Just Explicit -> return ()
            Just (Implicit r) -> do
                c <- sourceColumn <$> getPosition
                let compare = case ord of
                        Eq -> (==)
                        Gt -> (>)
                        Geq -> (>=)
                unless (c `compare` r) (
                    if ord == Eq
                    then unexpected "indentation"
                          Parsec.<?> "indentation of " ++ show r ++
                                     " (got " ++ show c ++ ")"
                    else unexpected "indentation")

token :: Token -> Parser Lexeme
token t = satisfy (== t) <?> showTokenPretty t

oneOf :: [Token] -> Parser Lexeme
oneOf ts = satisfy (`elem` ts)

noneOf :: [Token] -> Parser Lexeme
noneOf ts = satisfy (`notElem` ts)

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

optSemi :: Parser ()
optSemi = optional semicolon

stmtSep :: Parser a -> Parser [a]
stmtSep p = many semicolon >> p `sepEndBy` many1 semicolon

stmtSep1 :: Parser a -> Parser [a]
stmtSep1 p = many semicolon >> p `sepEndBy1` many1 semicolon

-----------------------------------------------------------------------------------------
-- Implementing Layout Sensitivity
-----------------------------------------------------------------------------------------

-- | Forces the next token to be aligned with the reference column.
-- Alignment will be checked even if the next token is not TokIndent.
align :: Parser ()
align = modify $ \s -> s { indentOrd = Eq }


-- | Allows the next token to be either aligned with the reference column or further indented.
maybeAlign :: Parser ()
maybeAlign = modify $ \s -> s { indentOrd = Geq }



-- | Parses a block of parser 'p'. A "block" can be either explicit or implicit.
-- An explicit block is surrounded by '{' and '}' and 'p' must be separated by semicolons.
-- An implicit block is denoted by layout rules, and separate instance of 'p' on the same
-- line (or linefolded) must be separated by semicolons.
block :: Parser a -> Parser [a]
block p = explicitBlock <|> implicitBlock
  where
    explicitBlock = between
        (token TokLBrace >> pushLayoutContext Explicit)
        (token TokRBrace >> popLayoutContext)
        $ stmtSep p
    implicitBlock = between openImplicit closeImplicit
        $ concat <$> many (align >> stmtSep1 p) <|> return []

-- | The same as 'block', but requires at least one instance of 'p'.
block1 :: Parser a -> Parser [a]
block1 p = explicitBlock1 <|> implicitBlock1
  where
    explicitBlock1 = between
        (token TokLBrace >> pushLayoutContext Explicit)
        (token TokRBrace >> popLayoutContext)
        $ stmtSep1 p

    implicitBlock1 = between openImplicit closeImplicit
        $ concat <$> many1 (align >> stmtSep1 p)

-- | Opens an implicit block. Always succeeds and modifies the LayoutContexts.
openImplicit = do
    c <- sourceColumn <$> getPosition
    pushLayoutContext $ Implicit c

-- | Closes an implicit block. Always succeeds and modifies the LayoutContexts.
closeImplicit = popLayoutContext

locate :: Parser a -> Parser (Located a)
locate p = do
    startPos <- getPosition
    let srcName = sourceName startPos
        startLine = sourceLine startPos
        startCol  = sourceColumn startPos
        startLoc  = mkSrcLoc (T.pack srcName) startLine startCol
    res <- p
    endPos <- gets endOfPrevToken
    return $ Located (mkSrcSpan startLoc endPos) res

-----------------------------------------------------------------------------------------
-- Running Parsers
-----------------------------------------------------------------------------------------

runParser :: Parser a -> SourceName -> Settings -> [Lexeme] -> Either ParseError a
runParser p srcname flags lexemes =
    Parsec.runParser p (initParseState flags) srcname lexemes

-- TODO: don't fail with a CDoc, fail with an ErrMsg
-- adjust lexer to fail with an ErrMsg as well.
-- and record the refactoring in WYAH.
testParser :: Parser a -> String -> Either Out.CDoc a
testParser p input = do
    lexemes <- mapLeft Out.text $ lex "" input
    case runParser (initPos *> p <* eof) "" defaultSettings lexemes of
        Right v  -> Right v
        Left err -> Left $ pprParseError err input lexemes
  where mapLeft :: (e -> e') -> Either e a -> Either e' a
        mapLeft f (Left e)  = Left (f e)
        mapLeft f (Right a) = Right a

initPos :: Parser ()
initPos = do
    input <- getInput
    startPos <- getPosition
    case input of
        [] -> return ()
        ls -> setPosition $ posFromTok startPos undefined ls

-----------------------------------------------------------------------------------------
-- Parse Names
-----------------------------------------------------------------------------------------

varid :: Parser ParsedName
varid = flip label "identifier" $ do
    Located loc tok <- satisfy isVarIdToken
    return $ case tok of
        TokVarId name          -> mkUnQual varName loc name
        TokQualVarId qual name -> mkQual varName loc (qual, name)

varsym :: Parser ParsedName
varsym = flip label "symbol" $ do
    Located loc tok <- satisfy isVarSymToken
    return $ case tok of
        TokVarSym name          -> mkUnQual varName loc name
        TokQualVarSym qual name -> mkQual varName loc (qual, name)

var :: Parser ParsedName
var = varid <|> try (parens varsym)

tyconid :: Parser ParsedName
tyconid = flip label "type constructor" $ do
    Located loc tok <- satisfy isConIdToken
    return $ case tok of
        TokConId name          -> mkUnQual tcName loc name
        TokQualConId qual name -> mkQual tcName loc (qual, name)

tyclsid :: Parser ParsedName
tyclsid = label tyconid "type class"

dataconid :: Parser ParsedName
dataconid = flip label "data constructor" $ do
    Located loc tok <- satisfy isConIdToken
    return $ case tok of
        TokConId name          -> mkUnQual dataName loc name
        TokQualConId qual name -> mkQual dataName loc (qual, name)

dataconsym :: Parser ParsedName
dataconsym = flip label "data constructor (symbol)" $ do
    Located loc tok <- satisfy isConSymToken
    return $ case tok of
        TokConSym name          -> mkUnQual dataName loc name
        TokQualConSym qual name -> mkQual dataName loc (qual, name)

tyvarid :: Parser ParsedName
tyvarid = flip label "type variable" $ do
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
-- not have created a 'TokLitChar' if it was not a lexically valid char.
charLiteral :: Parser Char
charLiteral = do
    Located _ (TokLitChar t) <- satisfy
                                (\case TokLitChar _ -> True; _ -> False)
    let Right c = Parsec.parse (PT.charLiteral literalParsers) "" t
    return c

stringLiteral :: Parser Text
stringLiteral = do
    Located _ (TokLitString t) <- satisfy
                                (\case TokLitString _ -> True; _ -> False)
    let Right s = Parsec.parse (PT.stringLiteral literalParsers) "" t
    return $ T.pack s


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
