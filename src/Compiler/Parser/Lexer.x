--
-- Lexical syntax for ProtoHaskell.
--
-- Modified from Simon Marlow's example file in the Alex source.


{
module Compiler.Parser.Lexer
    ( Lexeme, Token(..), lex
    , reservedIdToTok
    , reservedOpToTok
    , isVarIdToken
    , isVarSymToken
    , isConIdToken
    , isConSymToken
    ) where

import Prelude hiding (lex)
import Data.Char (isAlphaNum, chr)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T

import Compiler.BasicTypes.SrcLoc
import Utils.Outputable
}

%wrapper "monadUserState"

$whitechar = [\t\n\r\f\v\ ]
$special   = [\(\)\,\;\[\]\`\{\}]

$ascdigit  = 0-9
$unidigit  = []
$digit     = [$ascdigit $unidigit]

$ascsymbol = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~]
$unisymbol = []
$symbol    = [$ascsymbol $unisymbol] # [$special \_\"\']

$large     = [A-Z \xc0-\xd6 \xd8-\xde]
$small     = [a-z \xdf-\xf6 \xf8-\xff \_]
$alpha     = [$small $large]

$graphic   = [$small $large $symbol $digit $special \:\"\']

$octit     = 0-7
$hexit     = [0-9 A-F a-f]
$idchar    = [$alpha $digit \']
$symchar   = [$symbol \:]
$nl        = [\n\r]

@reservedid = case|class|data|default|deriving|do|else|if|
              import|in|infix|infixl|infixr|instance|let|module|newtype|
              of|then|type|where

@reservedop =
            ".." | ":" | "::" | "=" | \\ | "|" | "<-" | "->" | "@" | "~" | "=>" | "_"

@varid  = $small $idchar*
@conid  = $large $idchar*
@varsym = $symbol $symchar*
@consym = \: $symchar*

@decimal     = $digit+
@octal       = $octit+
@hexadecimal = $hexit+
@exponent    = [eE] [\-\+] @decimal

$cntrl   = [$large \@\[\\\]\^\_]
@ascii   = \^ $cntrl | NUL | SOH | STX | ETX | EOT | ENQ | ACK
         | BEL | BS | HT | LF | VT | FF | CR | SO | SI | DLE
         | DC1 | DC2 | DC3 | DC4 | NAK | SYN | ETB | CAN | EM
         | SUB | ESC | FS | GS | RS | US | SP | DEL
$charesc = [abfnrtv\\\"\'\&]
@escape  = \\ ($charesc | @ascii | @decimal | o @octal | x @hexadecimal)
@gap     = \\ $whitechar+ \\
@string  = $graphic # [\"\\] | " " | @escape | @gap

haskell :-

<0> $white+                   { skip }
<0> "--"\-*[^$symchar].*      { skip }

"{-"                          { nested_comment }

<0> $special                  { mkL TokTypeSpecial }

<0> @reservedid               { mkL TokTypeReservedId }
<0> @reservedop               { mkL TokTypeReservedOp } -- here because '_' overlaps varid

<0> (@conid \.)+ @varid       { mkL TokTypeQualVarId }
<0> (@conid \.)+ @conid       { mkL TokTypeQualConId }
<0> @varid                    { mkL TokTypeVarId }
<0> @conid                    { mkL TokTypeConId }

<0> (@conid \.)+ @varsym      { mkL TokTypeQualVarSym }
<0> (@conid \.)+ @consym      { mkL TokTypeQualConSym }
<0> @varsym                   { mkL TokTypeVarSym }
<0> @consym                   { mkL TokTypeConSym }

<0> @decimal
  | 0[oO] @octal
  | 0[xX] @hexadecimal        { mkL TokTypeInteger }


<0> @decimal \. @decimal @exponent?
  | @decimal @exponent        { mkL TokTypeFloat }

<0> \' ($graphic # [\'\\] | " " | @escape) \'
                              { mkL TokTypeChar }

<0> \" @string* \"            { mkL TokTypeString }

{
type AlexUserState = String
alexInitUserState = ""

type Lexeme = Located Token

data TokenType
     = TokTypeInteger
     | TokTypeFloat
     | TokTypeChar
     | TokTypeString
     | TokTypeSpecial
     | TokTypeReservedId
     | TokTypeReservedOp
     | TokTypeVarId
     | TokTypeQualVarId
     | TokTypeConId
     | TokTypeQualConId
     | TokTypeVarSym
     | TokTypeQualVarSym
     | TokTypeConSym
     | TokTypeQualConSym
     | TokTypeEOF
     deriving (Eq, Show)

mkL :: TokenType -> AlexInput -> Int -> Alex Lexeme
mkL toktype (alexStartPos,_,_,str) len = do
    fname <- T.pack <$> alexGetFilename
    alexEndPos <- alexGetPos
    let AlexPn _ startLine startCol = alexStartPos
        AlexPn _ endLine endCol = alexEndPos
        startPos = mkSrcLoc fname startLine startCol
        endPos   = mkSrcLoc fname endLine endCol
        srcSpan  = mkSrcSpan startPos endPos
        src = take len str
        cont = case toktype of
            TokTypeInteger    -> mkTok1 TokLitInteger
            TokTypeFloat      -> mkTok1 TokLitFloat
            TokTypeChar       -> mkTok1 TokLitChar
            TokTypeString     -> mkTok1 TokLitString
            TokTypeSpecial    -> mkSpecialTok
            TokTypeReservedId -> mkReservedIdTok
            TokTypeReservedOp -> mkReservedOpTok
            TokTypeVarId      -> mkTok1    TokVarId
            TokTypeQualVarId  -> mkTokQual TokQualVarId
            TokTypeConId      -> mkTok1    TokConId
            TokTypeQualConId  -> mkTokQual TokQualConId
            TokTypeVarSym     -> mkTok1    TokVarSym
            TokTypeQualVarSym -> mkTokQual TokQualVarSym
            TokTypeConSym     -> mkTok1    TokConSym
            TokTypeQualConSym -> mkTokQual TokQualConSym
            TokTypeEOF        -> mkTokEOF
    return $ cont srcSpan src

mkTok1 constructor srcSpan src =
    Located srcSpan (constructor $ T.pack src)

mkTokQual constructor srcSpan src =
    Located srcSpan (constructor qualifier name)
  where
    (qualifier, name) = let (q, n) = unqualify "" src
                         in (T.pack q, T.pack n)

    unqualify :: String -> String -> (String, String)
    unqualify qual ('.':c:rest)
      | (not . isAlphaNum) c = (qual, c:rest)
      | '.' `notElem` rest = (qual, c:rest)
      | otherwise = unqualify (qual ++ ".") (c:rest)
    unqualify qual (c:cs) = unqualify (qual ++ [c]) cs

mkSpecialTok srcSpan src = Located srcSpan $ case src of
    "(" -> TokLParen
    ")" -> TokRParen
    "," -> TokComma
    ";" -> TokSemicolon
    "[" -> TokLBracket
    "]" -> TokRBracket
    "{" -> TokLBrace
    "}" -> TokRBrace
    "`" -> TokBackquote

mkReservedIdTok srcSpan src = Located srcSpan $ reservedIdToTok src

reservedIdToTok = \case
    "case"     -> TokCase
    "class"    -> TokClass
    "data"     -> TokData
    "default"  -> TokDefault
    "deriving" -> TokDeriving
    "do"       -> TokDo
    "else"     -> TokElse
    "if"       -> TokIf
    "import"   -> TokImport
    "in"       -> TokIn
    "infix"    -> TokInfix
    "infixl"   -> TokInfixL
    "infixr"   -> TokInfixR
    "instance" -> TokInstance
    "let"      -> TokLet
    "module"   -> TokModule
    "newtype"  -> TokNewtype
    "of"       -> TokOf
    "then"     -> TokThen
    "type"     -> TokType
    "where"    -> TokWhere
    str        -> error $ "Compiler.Parser.Lexer.reservedIdToTok: " ++
        "String `" ++ show str ++ "' is not a reserved word."

mkReservedOpTok srcSpan src = Located srcSpan $ reservedOpToTok src

reservedOpToTok = \case
    ".." -> TokTwoDots
    ":"  -> TokColon
    "::" -> TokDoubleColon
    "="  -> TokEqual
    "\\" -> TokLambda
    "|"  -> TokBar
    "<-" -> TokLArrow
    "->" -> TokRArrow
    "@"  -> TokAt
    "~"  -> TokTilde
    "=>" -> TokPredArrow
    "_"  -> TokUnderscore
    str  -> error $ "Compiler.Parser.Lexer.reservedOpToTok: " ++
        "String `" ++ show str ++ "' is not a reserved operator."

mkTokEOF _ _ = Located undefined TokEOF

data Token
       -- ^ Special Characters
     = TokLParen
     | TokRParen
     | TokComma
     | TokSemicolon
     | TokLBracket
     | TokRBracket
     | TokLBrace
     | TokRBrace
     | TokBackquote
     | TokUnderscore -- Not truly special, as it's legal in identifiers

       -- ^ Literals
     | TokLitInteger Text
     | TokLitFloat   Text
     | TokLitChar    Text
     | TokLitString  Text

       -- ^ Reserved Words
     | TokCase   | TokClass   | TokData   | TokDefault  | TokDeriving
     | TokDo     | TokElse    | TokIf     | TokImport   | TokIn
     | TokInfix  | TokInfixL  | TokInfixR | TokInstance | TokLet
     | TokModule | TokNewtype | TokOf     | TokThen     | TokType
     | TokWhere

       -- ^ Reserved Operators
     | TokTwoDots -- ".."
     | TokColon | TokDoubleColon | TokEqual  | TokLambda
     | TokBar   | TokLArrow      | TokRArrow | TokAt
     | TokTilde | TokPredArrow

       -- ^ Other
     | TokVarId      Text
     | TokQualVarId  Text Text
     | TokConId      Text
     | TokQualConId  Text Text
     | TokVarSym     Text
     | TokQualVarSym Text Text
     | TokConSym     Text
     | TokQualConSym Text Text
     | TokEOF

     | TokIndent
     deriving (Eq, Ord, Show)

isVarIdToken :: Token -> Bool
isVarIdToken (TokVarId _) = True
isVarIdToken (TokQualVarId _ _) = True
isVarIdToken _ = False

isConIdToken :: Token -> Bool
isConIdToken (TokConId _) = True
isConIdToken (TokQualConId _ _) = True
isConIdToken _ = False

isVarSymToken :: Token -> Bool
isVarSymToken (TokVarSym _) = True
isVarSymToken (TokQualVarSym _ _) = True
isVarSymToken _ = False

isConSymToken :: Token -> Bool
isConSymToken (TokConSym _) = True
isConSymToken (TokQualConSym _ _) = True
isConSymToken _ = False

nested_comment :: AlexInput -> Int -> Alex Lexeme
nested_comment _ _ = do
    input <- alexGetInput
    go 1 input
  where go 0 input = alexSetInput input >> alexMonadScan
        go n input = do
            case alexGetByte input of
                Nothing -> err input
                Just (c, input) -> do
                    case chr (fromIntegral c) of
                        '-' -> let temp = input
                               in case alexGetByte input of
                                   Nothing -> err input
                                   Just (125, input) -> go (n-1) input
                                   Just (45,  input) -> go n temp
                                   Just (c, input)   -> go n input
                        '\123' -> case alexGetByte input of
                            Nothing -> err input
                            Just (c, input)
                              | c == fromIntegral (ord '-') -> go (n+1) input
                            Just (c, input) -> go n input
                        c -> go n input
        err input = alexSetInput input >> lexError "error in nested comment"

lexError s = do
    (p,c,_,input) <- alexGetInput
    alexError $ showPosn p ++ ": " ++ s ++
                      (if not $ null input
                       then " before " ++ show (head input)
                       else " at end of file")

alexInitFilename :: String -> Alex ()
alexInitFilename fname = Alex $ \s -> Right (s { alex_ust = fname }, ())

alexGetFilename :: Alex String
alexGetFilename = Alex $ \s -> Right (s, alex_ust s)

alexGetPos :: Alex AlexPosn
alexGetPos = Alex $ \s -> Right (s, alex_pos s)

lex :: String -> String -> Either String [Lexeme]
lex fname input = do
    lexemes <- runAlex input $ alexInitFilename fname >> init <$> alexLex
    return $ insertIndentationToks lexemes

alexLex :: Alex [Lexeme]
alexLex = do lexeme@(Located _ tok) <- alexMonadScan
             if tok == TokEOF
               then return $ [lexeme]
               else (lexeme:) <$> alexLex

alexEOF = return $ Located undefined TokEOF

insertIndentationToks :: [Lexeme] -> [Lexeme]
insertIndentationToks [] = []
insertIndentationToks (l@(Located srcSpan _) : ls) =
    (noLoc TokIndent) : go (l : ls)
  where go [] = []
        go [l] = [l]
        go (l1@(Located s1 _) : l2@(Located s2 _) : ls) =
            -- Test if token l2 is the first on its line, including the end of token l1
            if (unsafeLocLine $ srcSpanStart s2) > (unsafeLocLine $ srcSpanEnd s1)
            -- If it is, insert indent token
            then l1 : noLoc TokIndent : go (l2 : ls)
            else l1 : go (l2 : ls)

showPosn (AlexPn _ line col) = show line ++ ':' : show col

instance Outputable Token where
    ppr tok = text $ case tok of
        TokLParen -> "("
        TokRParen -> ")"
        TokComma  -> ","
        TokSemicolon -> ";"
        TokLBracket  -> "["
        TokRBracket  -> "]"
        TokLBrace    -> "{"
        TokRBrace    -> "}"
        TokBackquote -> "`"
        TokUnderscore -> "_"
        TokLitInteger i -> show i
        TokLitFloat f   -> show f
        TokLitChar c    -> show c
        TokLitString s  -> show s
        TokCase     -> "case"
        TokClass    -> "class"
        TokData     -> "data"
        TokDefault  -> "default"
        TokDeriving -> "deriving"
        TokDo       -> "do"
        TokElse     -> "else"
        TokIf       -> "if"
        TokImport   -> "import"
        TokIn       -> "in"
        TokInfix    -> "infix"
        TokInfixL   -> "infixl"
        TokInfixR   -> "infixr"
        TokInstance -> "instance"
        TokLet      -> "let"
        TokModule   -> "module"
        TokNewtype  -> "newtype"
        TokOf       -> "of"
        TokThen     -> "then"
        TokType     -> "type"
        TokWhere    -> "where"
        TokTwoDots  -> ".."
        TokColon    -> ":"
        TokDoubleColon -> "::"
        TokEqual    -> "="
        TokLambda   -> "\\"
        TokBar      -> "|"
        TokLArrow   -> "<-"
        TokRArrow   -> "->"
        TokAt       -> "@"
        TokTilde    -> "~"
        TokPredArrow -> "=>"
        TokVarId id           -> T.unpack id
        TokQualVarId qual id  -> T.unpack qual ++ "." ++ T.unpack id
        TokConId id           -> T.unpack id
        TokQualConId qual id  -> T.unpack qual ++ "." ++ T.unpack id
        TokVarSym sym         -> T.unpack sym
        TokQualVarSym qual id -> T.unpack qual ++ "." ++ T.unpack id
        TokConSym sym         -> T.unpack sym
        TokQualConSym qual id -> T.unpack qual ++ "." ++ T.unpack id
        TokEOF -> "<end of file>"
        TokIndent -> "<indentation>"
}
