module Compiler.Parser.Pattern (parse, parseLocated) where

import Compiler.Parser.Helpers
import Compiler.PhSyn.PhExpr

type PatternParser = Parser (Pat ParsedName)

parseLocated :: Parser (LPat ParsedName)
parseLocated = locate parse

parse :: PatternParser
parse = startsWithVar
    <|> literalPattern
    <|> token TokUnderscore $> PWild
    <|> PCon <$> dataconid <*> many parse
    <|> startsWithParen
    <|> PList <$> brackets (commaSep parse)
    <?> "pattern"

startsWithVar :: PatternParser
startsWithVar = do
    var <- varid
    asPat <- optionMaybe $ token TokAt *> parse
    return $ case asPat of
        Nothing  -> PVar var
        Just pat -> PAs var pat

literalPattern :: PatternParser
literalPattern = (PLit <$>) $
        LitInt <$> integer
    <|> LitFloat <$> float
    <|> LitChar <$> charLiteral
    <|> LitString <$> stringLiteral

startsWithParen :: PatternParser
startsWithParen = do
    inside <- parens $ many parse
    return $ case inside of
        -- [] -> PCon (getParsedName PhcUnitConName) []
        -- We can't uncomment the above until we can wire-in types
        -- In fact, we may want to instead adjust dataconid to accept '()'
        -- and then forget about it here.
        [pat] -> ParPat pat
        pats  -> PTuple pats
