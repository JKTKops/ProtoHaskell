module Compiler.Parser.Parser
    (
    ) where

import Prelude hiding (lex)

import Compiler.Parser.Lexer
import Compiler.Parser.Errors
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

import Data.Maybe (catMaybes, fromJust, isJust, maybe)

import Control.Arrow ((>>>))
import Data.Function ((&))
import Data.Functor  (($>))
import Control.Monad
import Control.Monad.Identity (Identity)

import Text.Parsec hiding ( runParser, parse, anyToken, token, satisfy, oneOf, noneOf
                          , label, (<?>))
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Error as Parsec
import Text.Parsec.Language (GenLanguageDef(..))
import Text.Parsec.Token    (GenTokenParser)
import qualified Text.Parsec.Token as PT
import qualified Text.Parsec.Prim as Prim (token)
import Text.Parsec.Pos (newPos)

import Debug.Trace

type Parser a = Parsec
                  [Lexeme]           -- Token stream
                  ParseState
                  a

data ParseState = ParseState
    { compFlags      :: Flags
    , indentOrd      :: Ordering
    , layoutContexts :: [LayoutContext]
    , endOfPrevToken :: SrcLoc
    }
    deriving (Show)

data LayoutContext
     = Explicit
     | Implicit Int
     deriving (Eq, Ord, Show)

initParseState flags = ParseState flags EQ [] noSrcLoc

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
                EQ -> show n
                GT -> "greater than " ++ show n
                LT -> "less than" ++ show n -- Shouldn't happen
            indentPiece = "at indentation"
        Parsec.label p $ unwords [exp, indentPiece, ordPiece]

(<?>) = label
infixl 0 <?> -- I disagree with this fixity but it's what Parsec uses

instance HasCompFlags (Parsec [Lexeme] ParseState) where
    getCompFlags = compFlags <$> getState

{- NOTE: [Overlapping Show instance for Lexeme]

For some reason, some parsec combinators seem to enforce a Show instance on Lexeme
which they then use inside `unexepctected` messages. This is catastrophic!
Bonus points for switching to `Megaparsec` if version 8 can handle custom streams properly.

To remedy this, we have to overlap the existing (informative) show instance for Lexeme
in order to be able to guarantee that the user sees pretty-printed error messages.

-}
instance {-# OVERLAPPING #-} Show (GenLocated SrcSpan Token) where
    show (Located _ TokIndent) = "indentation"
    show t = (Out.ppr >>> Out.prettyQuote >>> show) t

-----------------------------------------------------------------------------------------
-- Primitive parsers for our Tokens
-----------------------------------------------------------------------------------------

satisfy :: (Token -> Bool) -> Parser Lexeme
satisfy p = try $ guardIndentation *> satisfyNoIndentGuard p <* setIndentOrdGT
  where setIndentOrdGT = modify $ \s -> s { indentOrd = GT }

satisfyNoIndentGuard :: (Token -> Bool) -> Parser Lexeme
satisfyNoIndentGuard p = do
    lexeme@(Located pos _) <- Parsec.tokenPrim
                              (unLoc >>> Out.ppr >>> Out.prettyQuote >>> show)
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
    when (isJust check || ord == EQ) $ do
        mr <- currentLayoutContext
        case mr of
            Nothing -> return ()
            Just Explicit -> return ()
            Just (Implicit r) -> do
                c <- sourceColumn <$> getPosition
                when (c `compare` r /= ord) (mzero
                                 <?> "indentation of " ++ show r ++
                                     " (got " ++ show c ++ ")")

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
-- Implementing Layout Sensitivity
-----------------------------------------------------------------------------------------

align :: Parser ()
align = modifyState $ \s -> s { indentOrd = EQ }

-- block, block1 :: Parser a -> Parser [a]
-- block  p = laidout (concat <$> many  (align >> sepEndBy1 p (many1 semicolon)))
-- block1 p = laidout (concat <$> many1 (align >> sepEndBy1 p (many1 semicolon)))

block :: Parser a -> Parser [a]
block p = (catMaybes <$>) $ explicitBlock <|> implicitBlock
  where
    explicitBlock = between
        (token TokLBrace >> pushLayoutContext Explicit)
        (token TokRBrace >> popLayoutContext)
        $ optionMaybe p `sepBy` semicolon
    implicitBlock = between openImplicit closeImplicit $ many (align >> Just <$> p)

block1 :: Parser a -> Parser [a]
block1 p = explicitBlock1 <|> implicitBlock1
  where
    explicitBlock1 = between
        (token TokLBrace >> pushLayoutContext Explicit)
        (token TokRBrace >> popLayoutContext)
        $ many semicolon *> p `sepEndBy1` many1 semicolon

    implicitBlock1 = between openImplicit closeImplicit $ many1 (align >> p)

openImplicit = do
    c <- sourceColumn <$> getPosition
    pushLayoutContext $ Implicit c

closeImplicit = popLayoutContext

locate :: Parser a -> Parser (Located a)
locate p = do
    startPos <- getPosition
    let srcName = sourceName startPos
        startLine = sourceLine startPos
        startCol  = sourceColumn startPos
        startLoc  = mkSrcLoc (T.pack srcName) startLine startCol
    res <- p
    endPos <- endOfPrevToken <$> getState
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
    Parsec.runParser p (initParseState flags) srcname lexemes

testParser :: Parser a -> String -> Either String a
testParser p input = do
    lexemes <- lex "" input
    case runParser (initPos *> p <* eof) "" noFlags lexemes of
        Right v  -> Right v
        Left err -> Left (show (pprParseError err input lexemes))

initPos :: Parser ()
initPos = do
    input <- getInput
    startPos <- getPosition
    case input of
        [] -> return ()
        ls -> setPosition $ posFromTok startPos undefined ls

-----------------------------------------------------------------------------------------
-- Component Parsers
-----------------------------------------------------------------------------------------

modl :: Parser (PhModule ParsedName)
modl = Module <$> optionMaybe modlHeader <*> parseTopDecls

modlHeader :: Parser (Located Text)
modlHeader = do
    reserved "module"
    name <- modlName
    reserved "where"
    return name

parseTopDecls :: Parser [LPhDecl ParsedName]
parseTopDecls = block parseTopDecl

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
