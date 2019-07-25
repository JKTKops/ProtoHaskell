{-# LANGUAGE TypeFamilies, RecordWildCards #-}
module Compiler.Parser.MegaParser where

import Prelude hiding (lex)

import Compiler.Parser.Lexer
import Compiler.BasicTypes.SrcLoc
import Compiler.BasicTypes.ParsedName
import Compiler.BasicTypes.OccName

import Utils.Outputable hiding (comma, semicolon)
import qualified Utils.Outputable as O

import Data.Proxy
import Data.Void
import Text.Megaparsec hiding (State, Token, token, satisfy, parse)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char.Lexer as L

import Control.Category ((>>>))
import Data.Functor
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Fix (MonadFix)
import Control.Monad.Fail (MonadFail)
import Data.String

import Data.List (span)
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import Data.Text.Lazy (Text, unpack)
import Data.Function ((&))
import Data.Maybe (catMaybes)

import qualified Text.Megaparsec.Debug as D
import Debug.Trace



data TokStream = TokStream { lexemes :: [Lexeme]
                           , source  :: String
                           }
  deriving (Eq, Ord)

instance Show TokStream where
    show TokStream {lexemes} = show lexemes

instance Stream TokStream where
    type Token  TokStream = Lexeme
    type Tokens TokStream = [Lexeme]

    tokensToChunk Proxy lexemes = lexemes
    chunkToTokens Proxy lexemes = lexemes
    chunkLength   Proxy lexemes = length lexemes
    chunkEmpty    Proxy lexemes = null lexemes
    take1_ s@TokStream{lexemes} = case lexemes of
        []     -> Nothing
        (l:ls) -> Just (l, s{lexemes = ls})
    takeN_ n s@TokStream{lexemes}
      | n <= 0       = Just ([], s)
      | null lexemes = Nothing
      | otherwise    =
          let (xs, ls) = splitAt n lexemes
              in Just (xs, s{lexemes = ls})
    takeWhile_ p s@TokStream{lexemes} =
        let (xs, ls) = span p lexemes
        in (xs, s{lexemes = ls})
    showTokens Proxy = render
                     . fsep
                     . punctuate O.comma
                     . NE.toList
                     . fmap (prettyQuote . ppr)
    reachOffset o pst@PosState{..} =
        let rest = drop (o - pstateOffset) (lexemes pstateInput)
            nextPos = case rest of
                [] -> pstateSourcePos
                (Located _ TokIndent : x@(Located s _) : _) ->
                    mkSrcPos $ srcSpanStart s
                (x:_) -> mkSrcPos $ srcSpanStart $ getLoc x
            currentLine = unPos (sourceLine nextPos) - 1
            cLineSrc = case lines (source pstateInput) of
                [] -> "<empty source>"
                ls -> case ls !! currentLine of
                    "" -> "<empty line>"
                    s  -> s
        in (nextPos, cLineSrc, pst { pstateInput = TokStream rest (source pstateInput) })

mkSrcPos :: SrcLoc -> SourcePos
mkSrcPos srcLoc = SourcePos
                (unpack $ unsafeLocFile srcLoc)
                (mkPos $ unsafeLocLine  srcLoc)
                (mkPos $ unsafeLocCol   srcLoc)

type Parser a = ParsecT CustomError TokStream (State PState) a

data CustomError
    = PopEmptyLayoutCtx
    deriving (Eq, Ord, Show, Enum, Bounded)

instance ShowErrorComponent CustomError where
    showErrorComponent PopEmptyLayoutCtx =
        "Tried to pop a layout context, but there are no layout contexts"


data PState = PState { layout_ctx  :: [LayoutContext]
                     , lineFoldRef :: Maybe Pos       -- The line on which a linefold began
                     , onFreshLine :: Bool
                     }

data LayoutContext = Explicit | Implicit Pos deriving (Eq, Ord, Show)

initPState :: PState
initPState = PState [] Nothing True

currentLayoutContext :: Parser (Maybe LayoutContext)
currentLayoutContext = (\case [] -> Nothing; (x:_) -> Just x) <$> gets layout_ctx

pushLayoutContext :: LayoutContext -> Parser ()
pushLayoutContext ctx =
    modify $ \s@PState{ layout_ctx } -> s { layout_ctx = ctx : layout_ctx }

popLayoutContext :: Parser ()
popLayoutContext = do
    ctx <- gets layout_ctx
    case ctx of
        [] -> customFailure PopEmptyLayoutCtx
        (_:ctxs) -> modify $ \s -> s { layout_ctx = ctxs }

parse :: Parser a -> String -> String -> Either String a
parse p fname source = do
    lexemes <- lex fname source
    let out = runParserT p fname (TokStream lexemes source)
              & flip evalState initPState
    case out of
        Left errBundle -> Left $ errorBundlePretty errBundle
        Right val      -> Right val

testParser :: Parser a -> String -> Either String a
testParser p input = parse p "" input

-- updatePos :: Parser ()
-- updatePos = do
--     ls <- lexemes <$> getInput
--     prevLine <- unPos . sourceColumn <$> getSourcePos
--     unless (null ls) $ do
--     let srcSpan = case ls of
--             (Located _ TokIndent : Located s _ : _) -> s
--             (Located s _ : _) -> s
--         nextLoc = srcSpanStart srcSpan
--         nextSrcPos = mkSrcPos nextLoc
--     updateParserState (\s@P.State{ statePosState } ->
--         let newStatePosState = statePosState{ pstateSourcePos = nextSrcPos }
--         in s{ statePosState = newStatePosState })

satisfy :: (Token -> Bool) -> Parser Lexeme
satisfy p = try $ optional guardIndentation >> P.satisfy (p . unLoc)

guardIndentation :: Parser ()
guardIndentation = D.dbg "guardIndentation" $ do
    P.token
         (\(unLoc -> t) -> if t == TokIndent then Just TokIndent else Nothing)
         Set.empty
    mlc <- currentLayoutContext
    case mlc of
        Nothing -> return ()
        Just Explicit -> return ()
        Just (Implicit indent) -> implicit indent
  where
    implicit :: Pos -> Parser ()
    implicit indent = do
        mlf <- D.dbg "gets lineFoldRef" $ gets lineFoldRef
        case mlf of
            Nothing -> void $ L.indentGuard (return ()) EQ indent
            Just lfr -> lineFold lfr indent

    lineFold :: Pos -> Pos -> Parser ()
    lineFold lineFoldRef indent = do
        currentLine <- sourceLine <$> getSourcePos
        if currentLine == lineFoldRef
        then void $ L.indentGuard (return ()) EQ indent
        else void $ L.indentGuard (return ()) GT indent

token :: Token -> Parser Lexeme
token t = satisfy (== t) <?> ppr t & prettyQuote & show

---------------------------------------------------------------------------------------
-- Helper Combinators
---------------------------------------------------------------------------------------

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

block :: Parser a -> Parser [a]
block p = (catMaybes <$>) $
    explicitBlock <|> implicitBlock
  where
    explicitBlock = between openExplicit closeExplicit $
        optional p `sepBy` semicolon

    implicitBlock = between openImplicit closeImplicit $
        P.many $ try (lineFold $ Just <$> p)

openExplicit = do
    token TokLBrace
    pushLayoutContext Explicit

closeExplicit = do
    token TokRBrace
    popLayoutContext

openImplicit = do
    current <- L.indentLevel
    pushLayoutContext $ Implicit current

closeImplicit = popLayoutContext

lineFold :: Parser a -> Parser a
lineFold = between setLineFoldRef unsetLineFoldRef
  where
    setLineFoldRef = do
        D.dbg "input" $ do
            st <- getParserState
            return (stateOffset st, statePosState st)
        currentLine <- D.dbg "get reference for linefold" $ sourceLine <$> getSourcePos
        modify $ \s -> s { lineFoldRef = Just currentLine }

    unsetLineFoldRef = modify $ \s -> s { lineFoldRef = Nothing }
