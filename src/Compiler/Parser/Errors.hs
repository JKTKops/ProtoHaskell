module Compiler.Parser.Errors (pprParseError) where

import Text.Parsec.Error
import Text.Parsec.Pos

import Utils.Outputable
import Compiler.BasicTypes.SrcLoc
import Compiler.Parser.Lexer

import Data.Char

pprParseError :: ParseError -> String -> [Lexeme] -> Doc
pprParseError pe "" [] = text $ show pe -- Use text . show instead of ppr in case
                                        -- definition of ppr changes to use this function
pprParseError pe "" _  = text $ show pe
pprParseError pe _  [] = text $ show pe
pprParseError pe src lexemes =
    let pos  = errorPos pe
        msgs = errorMessages pe
        line = sourceLine pos
        col  = sourceColumn pos
        info = findInfo line col src lexemes
        infoAmount = case info of
            (Nothing, Nothing) -> NoInfo
            (Just _,  Nothing) -> Source
            (Just _,  Just _)  -> SourceAndLexeme
            (Nothing, Just _)  -> Lexeme
        spaces = replicate (length (show line) + 1) ' '
        template src arrows = spaces ++ "|\n" ++
                              show line ++ " | " ++ src ++ "\n" ++
                              spaces ++ "| " ++ arrows
    in mkErrorMessage infoAmount info pos msgs template

data InfoAmount = NoInfo | Source | SourceAndLexeme | Lexeme

findInfo :: Line -> Column -> String -> [Lexeme] -> (Maybe String, Maybe Lexeme)
findInfo line col src lexs =
    (searchString src line col, searchLexemes lexs line col)

searchString :: String -> Line -> Column -> Maybe String
searchString src line col = do
    let srcLines = lines src
    if length srcLines < line
      then Nothing
      else Just $ srcLines !! (line - 1)

searchLexemes :: [Lexeme] -> Line -> Column -> Maybe Lexeme
searchLexemes lexs line col =
    let assocList = map (\l@(Located span _) ->
                             let start = srcSpanStart span
                             in ((unsafeLocLine start, unsafeLocCol start), l))
                    . filter (\(Located _ t) -> t /= TokIndent)
                    $ lexs
    in lookup (line, col) assocList

showPos :: SourcePos -> String
showPos p = show (sourceLine p) ++ ":" ++ show (sourceColumn p)

mkErrorMessage :: InfoAmount                   -- How detailed can our source/arrows be
               -> (Maybe String, Maybe Lexeme) -- Components for source/arrows
               -> SourcePos                    -- Position of error
               -> [Message]                    -- Parsec error messages
               -> (String -> String -> String) -- Callback to template
                                                 -- source/arrows into msg
               -> Doc
mkErrorMessage infoAmt info pos msgs template =
    let prettySource = case infoAmt of
            NoInfo -> mempty
            Source -> templateSource
            SourceAndLexeme -> templateSourceAndLexeme
            Lexeme -> templateLexeme
    in header pos $$ prettySource $$ errMsgBody msgs
  where
    getBodyAndArrowWs src =
        let col = sourceColumn pos
            (leadingWS, body) = span isSpace src
            initSrcLoc = mkRealSrcLoc "" (sourceLine pos) 1
            bodyStartLoc = foldl advanceSrcLoc initSrcLoc leadingWS
            arrowWs = replicate (col - realSrcLocCol bodyStartLoc) ' '
        in (body, arrowWs)

    templateSource =
        let (Just src, _) = info
            (body, arrowWs) = getBodyAndArrowWs src
            arrows = "^"
        in text (template body (arrowWs ++ arrows))

    templateSourceAndLexeme =
        let (Just src, Just (Located (RealSrcSpan span) _)) = info
            (body, arrowWs) = getBodyAndArrowWs src
            mNumArrows = realSrcSpanLength span
            --arrows = replicate (realSrcSpanLength span) '^'
        in case mNumArrows of
            Nothing -> "<Can't display source>"
            Just num -> text (template body (arrowWs ++ replicate num '^'))

    templateLexeme =
        let (_, Just (Located _ token)) = info
            body = output token
            arrows = replicate (length body) '^'
        in case length (lines body) of
            1 -> text $ template body arrows
            _ -> "<Can't display source>"

header :: SourcePos -> Doc
header pos = text $ "Parse error at " ++ showPos pos ++ ":"

errMsgBody :: [Message] -> Doc
errMsgBody = text . showErrorMessages "or" "unknown parse error"
                                      "expecting" "unexpected" "end of input"