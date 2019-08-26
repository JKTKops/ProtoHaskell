module Compiler.Parser.Parser
    (
    ) where

import Prelude hiding (lex)

import Compiler.Parser.Helpers
import qualified Compiler.Parser.Pattern as Pattern

import Compiler.PhSyn.PhSyn
import Compiler.PhSyn.PhExpr
import Compiler.PhSyn.PhType

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T

import Data.Function ((&))
import Control.Monad.Identity (Identity)

import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Error as Parsec

-----------------------------------------------------------------------------------------
-- Main parse driver
-----------------------------------------------------------------------------------------

-- | Takes a filename, compiler flags, and input source.
-- Outputs either an error (already pretty printed) or a PhModule.
parse :: SourceName -> Flags -> String -> Either String (PhModule ParsedName)
parse srcname flags input = do
    lexemes <- lex srcname input
    case runParser modl srcname flags lexemes of
        Right modl -> Right modl
        Left parseErr -> Left (show parseErr)

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

-- TODO: parse bindings for operators
parseFunBinding :: Parser (PhBind ParsedName)
parseFunBinding = do
    funName <- varid
    match   <- locate $ parseMatch FunCtxt
    return $ FunBind funName $ MG { mgAlts = [match], mgCtxt = FunCtxt }

parsePatternBinding :: Parser (PhBind ParsedName)
parsePatternBinding = PatBind <$> Pattern.parseLocated <*> locate (parseRHS LetCtxt)

-- | Parses a single match which will eventually be part of a match group.
--
-- Considers the context as follows:
--
-- Lam contexts are not allowed to contain local bindings.
--
-- See also: 'parseRHS'
parseMatch :: MatchContext -> Parser (Match ParsedName)
parseMatch ctx = do
    pats <- many1 Pattern.parseLocated
    rhs  <- locate $ parseRHS ctx
    mLocalBinds <- if ctx /= LamCtxt
        then optionMaybe $ token TokWhere >> parseLocalBinds
        else return Nothing
    return Match { matchPats = pats, rhs, localBinds = mLocalBinds }

-- | Parses the right hand side of a binding. Considers the context as follows:
--
-- The left and right hand side are separated by '->' in Lam and Case contexts,
-- but by '=' in Fun and Let contexts
--
-- Lam contexts are not allowed to contain guards.
parseRHS :: MatchContext -> Parser (RHS ParsedName)
parseRHS LamCtxt = reservedOp "->" >> Unguarded <$> parseLocExpr
parseRHS ctxt = parseGuarded <|> parseUnguarded
  where parseBinder :: Parser ()
        parseBinder = matchCtx2Parser ctxt

        parseGuarded :: Parser (RHS ParsedName)
        parseGuarded = reservedOp "|" >> Guarded <$> parseGuard `sepBy1` reservedOp "|"

        parseGuard :: Parser (LGuard ParsedName)
        parseGuard = locate $ do
            guard <- parseLocExpr
            parseBinder
            rhs <- parseLocExpr
            return $ Guard guard rhs

        parseUnguarded = parseBinder >> Unguarded <$> parseLocExpr

type LocalDecls = ([LPhBind ParsedName], [LSig ParsedName])
parseLocalBinds :: Parser (LPhLocalBinds ParsedName)
parseLocalBinds = locate $ do
    decls <- block parseLocalDecl
    let (binds, sigs) = separate decls
    return $ LocalBinds binds sigs

  where
    -- The use of foldr is absolutely critical, as the order of the bindings matters
    -- folding from the right and prepending with (:) will maintain the order.
    -- The renamer should check the order *anyway* if the compiler is in debug mode.
    separate :: [LPhDecl ParsedName] -> LocalDecls
    separate = foldr move ([], [])

    move :: LPhDecl ParsedName -> LocalDecls -> LocalDecls
    move (Located s new) (binds, sigs) = case new of
        Binding bind  -> (Located s bind : binds, sigs)
        Signature sig -> (binds, Located s sig : sigs)

-- | Takes the pair of signatures and bindings already parsed
--   and parses a new one, placing it appropriately
parseLocalDecl :: Parser (LPhDecl ParsedName)
parseLocalDecl = parseSignature
             <|> parseBinding

matchCtx2Parser :: MatchContext -> Parser ()
matchCtx2Parser = void . \case
    FunCtxt -> reservedOp "="
    CaseCtxt -> reservedOp "->"
    LamCtxt -> reservedOp "->"
    LetCtxt -> reservedOp "="

-----------------------------------------------------------------------------------------
-- Parsing Expressions
-----------------------------------------------------------------------------------------

parseExpr :: Parser (PhExpr ParsedName)
parseExpr = do
    exp <- locate parseInfixExp
    mCType <- optionMaybe $ reservedOp "::" >> parseContextType
    return $ case mCType of
        Nothing -> unLoc exp
        Just ty -> Typed ty exp
   <?> "expression"

parseLocExpr :: Parser (LPhExpr ParsedName)
parseLocExpr = locate parseExpr

-- | Parses operator (infix) expressions.
-- We parse all operators as left associative and highest
-- precedence, then rebalance the tree later.
parseInfixExp :: Parser (PhExpr ParsedName)
parseInfixExp = parseSyntacticNegation
            <|> unLoc <$> locate parseBExp `chainl1` parseQopFunc

    where parseQopFunc = mkOpApp <$> parseQop
          mkOpApp op x@(Located left _) y@(Located right _) =
              Located (combineSrcSpans left right) (OpApp x op y)

parseQop :: Parser (LPhExpr ParsedName)
parseQop = locate (PhVar <$> (varsym <|> backticks varid) <?> "operator")

parseSyntacticNegation :: Parser (PhExpr ParsedName)
parseSyntacticNegation = do
    satisfy isDashSym
    e <- locate parseInfixExp
    return $ NegApp e
  where isDashSym :: Token -> Bool
        isDashSym (TokVarSym "-") = True
        isDashSym _ = False

parseBExp :: Parser (PhExpr ParsedName)
parseBExp = parseLamExp
        <|> parseLetExp
        <|> parseIfExp
        <|> parseCaseExp
        <|> parseDoExp
        <|> parseFExp

parseLamExp :: Parser (PhExpr ParsedName)
parseLamExp = do
    reservedOp "\\"
    match <- locate $ parseMatch LamCtxt -- parseMatch parses RHS
    return $ PhLam $ MG [match] LamCtxt

parseLetExp :: Parser (PhExpr ParsedName)
parseLetExp = do
    reserved "let"
    decls <- parseLocalBinds
    reserved "in"
    body <- parseLocExpr
    return $ PhLet decls body

parseIfExp :: Parser (PhExpr ParsedName)
parseIfExp = do
    reserved "if"
    c <- parseLocExpr
    optSemi
    maybeAlign >> reserved "then"
    t <- parseLocExpr
    optSemi
    maybeAlign >> reserved "else"
    f <- parseLocExpr
    return $ PhIf c t f

parseCaseExp :: Parser (PhExpr ParsedName)
parseCaseExp = do
    reserved "case"
    scrut <- parseLocExpr
    reserved "of"
    matchGroup <- parseCaseAlts
    return $ PhCase scrut matchGroup

parseDoExp :: Parser (PhExpr ParsedName)
parseDoExp = do
    reserved "do"
    PhDo <$> parseDoStmts

parseFExp :: Parser (PhExpr ParsedName)
parseFExp = unLoc <$> parseAExp `chainl1` return mkLPhAppExpr

parseAExp :: Parser (LPhExpr ParsedName)
parseAExp = locate
           (PhVar <$> varid
        <|> parseGCon
        <|> parseLiteral
        <|> parens parseExprParen
        <|> brackets parseExprBracket
        <?> "term")

parseGCon :: Parser (PhExpr ParsedName)
parseGCon = PhVar <$> dataconid

parseLiteral :: Parser (PhExpr ParsedName)
parseLiteral = (PhLit <$>) $
     LitInt <$> integer
 <|> LitFloat <$> float
 <|> LitChar <$> charLiteral
 <|> LitString <$> stringLiteral

parseExprParen :: Parser (PhExpr ParsedName)
parseExprParen = do
    exps <- commaSep1 parseLocExpr
    return $ case exps of
        -- () is a GCon
        [exp] -> PhPar exp
        exps  -> ExplicitTuple exps

parseExprBracket :: Parser (PhExpr ParsedName)
parseExprBracket = do
    exps <- commaSep1 $ locate parseExpr
    case exps of
        [exp] -> parseArithSeqInfo exp
        _     -> return $ ExplicitList exps

-- | Given the first expression (the e1 in [e1 ..], [e1, e2 ..], or [e1, e2 .. e3])
-- parse the rest of an arithmetic sequence description.
-- Handles the case of an explicit list with one element.
parseArithSeqInfo :: LPhExpr ParsedName -> Parser (PhExpr ParsedName)
parseArithSeqInfo exp = parseCommaSeq <|> parseDotsSeq <|> return (ExplicitList [exp])
  where parseCommaSeq = ArithSeq <$> do
            comma
            e2 <- parseLocExpr
            reservedOp ".."
            e3 <- optionMaybe parseLocExpr
            return $ case e3 of
                Nothing -> FromThen exp e2
                Just e  -> FromThenTo exp e2 e

        parseDotsSeq = ArithSeq <$> do
            reservedOp ".."
            e2 <- optionMaybe parseLocExpr
            return $ case e2 of
                Nothing -> From exp
                Just e  -> FromTo exp e

parseDoStmts :: Parser [LStmt ParsedName]
parseDoStmts = block1 parseDoStmt

parseDoStmt :: Parser (LStmt ParsedName)
parseDoStmt = locate
       (try (do pat <- Pattern.parseLocated
                reservedOp "<-"
                e <- parseLocExpr
                return $ SGenerator pat e)
    <|> try (reserved "let" *> (SLet <$> parseLocalBinds))
    <|> SExpr <$> parseLocExpr
    <?> "statement of a do block")

parseCaseAlts :: Parser (MatchGroup ParsedName)
parseCaseAlts = MG <$> (block1 . locate $ parseMatch CaseCtxt) <*> pure CaseCtxt

-----------------------------------------------------------------------------------------
-- Parsing Types
-----------------------------------------------------------------------------------------

-- | Parses a type with context, like Eq a => a -> a -> Bool
parseContextType :: Parser (LPhType ParsedName)
parseContextType = locate $ do
    mctx <- optionMaybe $ try $ parseContext <* reservedOp "=>"
    ty <- parseType
    return $ case mctx of
        Nothing -> unLoc ty
        Just preds -> PhQualTy preds ty

-- | Parses a context, but NOT the predicate arrow, =>
parseContext :: Parser [Pred ParsedName]
parseContext =
    pure <$> parsePred <|> parens (commaSep parsePred)

-- | Parses an individual predicate, like Eq a
parsePred :: Parser (Pred ParsedName)
parsePred = parseSimplePred
        <|> do cls <- tyclsid
               ty  <- parens $ do
                   tyvar <- locate $ PhVarTy <$> tyvarid
                   args <- many1 parseAType
                   return $ foldl1 mkLPhAppTy (tyvar : args)
               return $ IsIn cls (unLoc ty)

-- | Parses a simple context, like those legal in an instance head.
-- Unlike 'parseContext', rejects types like 'Eq (m a)'
parseSimpleContext :: Parser [Pred ParsedName]
parseSimpleContext =
    pure <$> parseSimplePred <|> parens (commaSep parseSimplePred)

-- | Parses a simple predicate, like 'Eq a'
parseSimplePred :: Parser (Pred ParsedName)
parseSimplePred = IsIn <$> tyclsid <*> (PhVarTy <$> tyvarid)

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

