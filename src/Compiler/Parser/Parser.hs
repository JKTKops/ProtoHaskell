module Compiler.Parser.Parser
    ( parse
    ) where

import Prelude hiding (lex)

import Utils.Outputable as Out (CDoc, string)

import Compiler.Parser.Errors
import Compiler.Parser.Helpers
import qualified Compiler.Parser.Pattern as Pattern

import Compiler.PhSyn.PhSyn
import Compiler.PhSyn.PhExpr
import Compiler.PhSyn.PhType

import Data.Text.Lazy (Text)

-----------------------------------------------------------------------------------------
-- Main parse driver
-----------------------------------------------------------------------------------------

-- | Takes a filename, compiler flags, and input source.
-- Outputs either an error (already pretty printed) or a PhModule.

-- TODO: fail with ErrMsg instead of CDoc
parse :: SourceName -> Settings -> String -> Either CDoc (PhModule ParsedName)
parse srcname flags input = do
    lexemes <- mapLeft Out.string $ lex srcname input
    case runParser (modl <* eof) srcname flags lexemes of
        Right modl -> Right modl
        Left parseErr -> Left $ pprParseError parseErr input lexemes
  where mapLeft :: (e -> e') -> Either e a -> Either e' a
        mapLeft f (Left e)  = Left (f e)
        mapLeft _ (Right a) = Right a

-----------------------------------------------------------------------------------------
-- Component Parsers
-----------------------------------------------------------------------------------------
{-HLINT ignore "Use <$>" -}

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
           <|> parseClassDecl
           <|> parseInstanceDecl
           <?> "top-level declaration"

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
    fields <- map unLoc <$> many parseAType -- don't want to use parseType, it will just see
                                            -- AppTy before many can turn it into a list
    return $ ConDecl name fields

parseSignature :: Parser (LPhDecl ParsedName)
parseSignature = locate . fmap Signature
    $ parseFixitySignature <|> try parseTypeSignature

parseTypeSignature :: Parser (Sig ParsedName)
parseTypeSignature = do
    names <- commaSep1 (varid <|> parens varsym)
    reservedOp "::"
    TypeSig names <$> parseContextType

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

emptyLocalBinds :: PhLocalBinds ParsedName
emptyLocalBinds = LocalBinds [] []

parseClassDecl :: Parser (LPhDecl ParsedName)
parseClassDecl = locate $ do
    reserved "class"
    ctx <- try (parseSimpleContext <* reservedOp "=>") <|> return []
    ClassDecl ctx
              <$> tyclsid -- renamer has to check that this is unqualified
              <*> tyvarid
              <*> ((reserved "where" *> parseLocalBinds) <|> locate (return emptyLocalBinds))

parseInstanceDecl :: Parser (LPhDecl ParsedName)
parseInstanceDecl = locate $ do
    reserved "instance"
    ctx <- try (parseSimpleContext <* reservedOp "=>") <|> return []
    InstDecl ctx <$> tyclsid
                 <*> parseType
                 <*> ((reserved "where" *> parseLocalBinds) <|> locate (return emptyLocalBinds))

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
    return Match { matchPats = pats, rhs }

-- | Parses the right hand side of a binding.
-- Lam contexts are not allowed to contain local bindings. Case contexts /are/, though
-- hardly anyone ever uses that feature.
parseRHS :: MatchContext -> Parser (RHS ParsedName)
parseRHS ctx = do
    grhs <- locate $ parseGRHS ctx
    mLocalBinds <- if ctx /= LamCtxt
        then optionMaybe (token TokWhere >> parseLocalBinds) <?> "where clause"
        else return Nothing
    return RHS { grhs, localBinds = flatten mLocalBinds }
  where flatten (Just binds) = binds
        flatten Nothing      = Located noSrcSpan $ LocalBinds [] []

-- | Parses the "guarded right hand sides" of a binding. See NOTE: [GRHS] in PhExpr.
--
-- The left and right hand side are separated by '->' in Lam and Case contexts,
-- but by '=' in Fun and Let contexts
--
-- Lam contexts are not allowed to contain guards.
parseGRHS :: MatchContext -> Parser (GuardedRHS ParsedName)
parseGRHS LamCtxt = do
    matchCtx2Parser LamCtxt
    Unguarded <$> parseLocExpr
parseGRHS ctxt = parseGuarded <|> parseUnguarded
  where parseBinder :: Parser ()
        parseBinder = matchCtx2Parser ctxt

        parseGuarded :: Parser (GuardedRHS ParsedName)
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
    -- The use of foldr is absolutely critical, as the order of the bindings matters.
    -- Folding from the right and prepending with (:) will maintain the order.
    -- The renamer should check the order *anyway* if the compiler is in debug mode.
    separate :: [LPhDecl ParsedName] -> LocalDecls
    separate = foldr move ([], [])

    -- | Takes the pair of signatures and bindings already parsed
    --   and parses a new one, placing it appropriately
    move :: LPhDecl ParsedName -> LocalDecls -> LocalDecls
    move (Located s new) (binds, sigs) = case new of
        Binding bind  -> (Located s bind : binds, sigs)
        Signature sig -> (binds, Located s sig : sigs)

parseLocalDecl :: Parser (LPhDecl ParsedName)
parseLocalDecl = parseSignature
                 -- We'll delay rejecting pattern bindings in class
                 -- or instance declarations for the renamer.
             <|> parseBinding
             <?> "local declaration"

matchCtx2Parser :: MatchContext -> Parser ()
matchCtx2Parser = void . \case
    FunCtxt  -> reservedOp "="  <|> failArrowSign <?> eqpretty
    CaseCtxt -> reservedOp "->" <|> failEqualSign <?> arpretty
    LamCtxt  -> reservedOp "->" <|> failEqualSign <?> arpretty
    LetCtxt  -> reservedOp "="  <|> failArrowSign <?> eqpretty
  where failEqualSign = anticipateOp "=" arpretty
        failArrowSign = anticipateOp "->" eqpretty

        eqpretty = showTokenPretty TokEqual
        arpretty = showTokenPretty TokRArrow

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
           (PhVar <$> var
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
        [exp] -> parseDotsSeq exp <|> return (ExplicitList [exp])
        [e1, e2] -> parseCommaSeq e1 e2 <|> return (ExplicitList [e1, e2])
        _     -> return $ ExplicitList exps

-- | Given the first two elements of an arithmetic sequence [e1, e2 ..] or [e1, e2 .. e3]
-- parse the rest.
parseCommaSeq :: LPhExpr ParsedName -> LPhExpr ParsedName -> Parser (PhExpr ParsedName)
parseCommaSeq e1 e2 = ArithSeq <$> do
    reservedOp ".."
    e3 <- optionMaybe parseLocExpr
    return $ case e3 of
         Nothing -> FromThen e1 e2
         Just e  -> FromThenTo e1 e2 e

parseDotsSeq :: LPhExpr ParsedName -> Parser (PhExpr ParsedName)
parseDotsSeq exp = ArithSeq <$> do
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
parsePred = try parseSimplePred
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
         <|> try parseGTyCon
         <|> do types <- parens $ commaSep1 parseType
                case types of
                    -- () is a GTyCon
                    [t] -> return $ PhParTy t
                    ts  -> return $ PhTupleTy ts
         <|> PhListTy <$> brackets parseType

parseGTyCon :: Parser (PhType ParsedName)
parseGTyCon = PhVarTy <$> tyconid
          <|>     (brackets (return ())     $> PhBuiltInTyCon ListTyCon)
          <|> try (parens (reservedOp "->") $> PhBuiltInTyCon FunTyCon)
          <|> do len <- length <$> parens (many comma)
                 return $ case len of
                     0 -> PhBuiltInTyCon UnitTyCon
                     _ -> PhBuiltInTyCon (TupleTyCon $ len + 1)
              -- TODO
              -- Once we have built-in representations for these types,
              -- the above should be replaced with those.
