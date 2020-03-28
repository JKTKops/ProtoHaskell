{-
Pretty printing.

This module used to be mainly of my own design, but the 'CDoc' version
is instead a HEAVILY modified version of GHC's 'Outputable' module.

The two modules are not interchangeable, due to differences in
naming preferences and the content of 'Flags'.
-}
module Utils.Outputable
    ( CDoc -- abstract
    , CDocContext(..), initCDocContext
    , CDocStyle(..), Depth(..), Colored(..), CodeStyle(..)
    , mkCodeStyle

    -- * Some convenience exports from Text.PrettyPrint.
    , module PrettyExports

    -- * Configure printing for a 'CDoc'
    , setCDocStyle, cdocDeeper, cdocDeeperList, cdocSetDepth
    , withCDocStyle, withCDocFlags, updCDocFlags, setCDocFlags
    , withIsDebugStyle, ifDebugStyle, whenDebugStyle

    -- * Common small 'CDoc's
    , semi, comma, colon, dcolon, equals, space, underscore
    , dot, vbar, arrow, darrow, larrow, lparen, rparen, lbrack
    , rbrack, lbrace, rbrace, backslash, blankLine

    -- * Basic 'CDoc' construction
    , empty, char, text, int, integer, float, double, rational
    , pprString

    -- * Common 'CDoc' combinators
    , parens, braces, brackets, quotes, doubleQuotes, angles
    , nest, (<+>), (<>), ($$), ($+$), hcat, hsep, vcat, sep
    , cat, fsep, fcat, punctuate

    , pprWhen, pprUnless , parensIf , prettyQuote , pprWithCommas
    , asPrefixVar, asInfixVar

    -- * Rendering to a handle
    , hPrintCDoc, hPrintCDocLn

    -- * Outputable class
    , Outputable(..)
    , output, outputWith, renderWithStyle
    ) where

import qualified Text.PrettyPrint as Pretty
import qualified Text.PrettyPrint as PrettyExports (Mode)
import Compiler.BasicTypes.Flags

import Data.Char
import System.IO

--------------------------------------------------
-- Imports for providing instances
--------------------------------------------------
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text.Lazy as Text
import qualified Text.Parsec as Parsec
import qualified Text.Megaparsec as MParsec

--------------------------------------------------
-- Exported Doc Type
--------------------------------------------------

newtype CDoc = CDoc { runCDoc :: CDocContext -> Pretty.Doc }

data CDocContext = CDocContext
     { cdocStyle :: !CDocStyle
     , cdocFlags :: !Flags
     -- coloring information coming soon
     }

initCDocContext :: Flags -> CDocStyle -> CDocContext
initCDocContext flags style = CDocContext style flags

data CDocStyle
     -- | Pretty-print for a user. This is for printing
     -- nice messages to the user, including the code in
     -- those messages if any.
     = UserStyle Depth Colored

     -- | Print full debugging output.
     | DebugStyle

     -- | Print compiler dumps. Less output than 'DebugStyle'
     -- but more than 'UserStyle'
     | DumpStyle

     -- | Print code produced by a code generator.
     -- Checking this can be useful if you target multiple output languages
     -- that share some types, like labels. The 'Outputable' instance for
     -- 'Label' should check the 'CodeStyle' to format appropriately, etc.
     | CodeStyle CodeStyle

data Depth = FullDepth
           | PartialDepth Int -- stop at 0
data Colored = Colored
             | Uncolored
data CodeStyle = CStyle    -- ^ Still don't know if we're targetting C or Java.
               | JavaStyle

isDebugStyle :: CDocStyle -> Bool
isDebugStyle DebugStyle = True
isDebugStyle _          = False

mkCodeStyle :: CodeStyle -> CDocStyle
mkCodeStyle = CodeStyle

instance Outputable CDocStyle where
    ppr UserStyle{}  = text "user-style"
    ppr DebugStyle{} = text "debug-style"
    ppr DumpStyle{}  = text "dump-style"
    ppr CodeStyle{}  = text "code-style"

instance Semigroup CDoc where
    (<>) = wrapper2 (<>)
instance Monoid CDoc where
    mempty = empty

--------------------------------------------------------------------------------------
-- Basic CDoc combinators
--------------------------------------------------------------------------------------

-- | Override the 'CDocStyle' for this 'CDoc'
setCDocStyle :: CDocStyle -> CDoc -> CDoc
setCDocStyle style d = CDoc $ \ctx -> runCDoc d ctx{cdocStyle = style}

cdocDeeper :: CDoc -> CDoc
cdocDeeper d = CDoc $ \ctx -> case ctx of
    CDocContext{cdocStyle = UserStyle (PartialDepth 0) _} -> Pretty.text "..."
    CDocContext{cdocStyle = UserStyle (PartialDepth n) c} ->
        runCDoc d ctx{cdocStyle = UserStyle (PartialDepth (n - 1)) c}
    _ -> runCDoc d ctx

-- | Truncate a list of 'CDoc' if it is longer than the current depth.
cdocDeeperList :: ([CDoc] -> CDoc) -> [CDoc] -> CDoc
cdocDeeperList f ds
  | null ds   = f []
  | otherwise = CDoc worker
  where worker ctx@CDocContext{cdocStyle = UserStyle (PartialDepth n) c}
          | n == 0    = Pretty.text "..."
          | otherwise = runCDoc (f (go 0 ds))
                          ctx{cdocStyle = UserStyle (PartialDepth (n - 1)) c}
          where go _ [] = []
                go i (d:ds)
                  | i >= n    = [text "..."]
                  | otherwise = d : go (i + 1) ds
        worker ctx = runCDoc (f ds) ctx

cdocSetDepth :: Depth -> CDoc -> CDoc
cdocSetDepth depth d = CDoc $ \ctx -> case ctx of
    CDocContext{cdocStyle = UserStyle _ c} ->
        runCDoc d ctx{cdocStyle = UserStyle depth c}
    _ -> runCDoc d ctx

withCDocStyle :: (CDocStyle -> CDoc) -> CDoc
withCDocStyle f = CDoc $ \ctx -> runCDoc (f $ cdocStyle ctx) ctx

withCDocFlags :: (Flags -> CDoc) -> CDoc
withCDocFlags f = CDoc $ \ctx -> runCDoc (f $ cdocFlags ctx) ctx

updCDocFlags :: (Flags -> Flags) -> CDoc -> CDoc
updCDocFlags upd d = CDoc $ \ctx ->
    runCDoc d (ctx{cdocFlags = upd (cdocFlags ctx)})

setCDocFlags :: Flags -> CDoc -> CDoc
setCDocFlags flags = updCDocFlags (const flags)

withIsDebugStyle :: (Bool -> CDoc) -> CDoc
withIsDebugStyle f = withCDocStyle $ f . isDebugStyle

ifDebugStyle :: CDoc -- ^ if we are in debug style
             -> CDoc -- ^ otherwise
             -> CDoc
ifDebugStyle yes no = withIsDebugStyle $ \b -> if b then yes else no

whenDebugStyle :: CDoc -> CDoc
whenDebugStyle d = ifDebugStyle d empty

--------------------------------------------------------------------------------------
--
-- Basic means of constructing CDocs
--
--------------------------------------------------------------------------------------

doc2CDoc :: Pretty.Doc -> CDoc
doc2CDoc = CDoc . const

empty    :: CDoc
char     :: Char     -> CDoc
text     :: String   -> CDoc
int      :: Int      -> CDoc
integer  :: Integer  -> CDoc
float    :: Float    -> CDoc
double   :: Double   -> CDoc
rational :: Rational -> CDoc

empty    = doc2CDoc   Pretty.empty
char     = doc2CDoc . Pretty.char
text     = doc2CDoc . Pretty.text
int      = doc2CDoc . Pretty.int
integer  = doc2CDoc . Pretty.integer
float    = doc2CDoc . Pretty.float
double   = doc2CDoc . Pretty.double
rational = doc2CDoc . Pretty.rational

wrapper :: (Pretty.Doc -> Pretty.Doc) -> CDoc -> CDoc
wrapper f d = CDoc $ f . runCDoc d
{-# INLINE wrapper #-}

wrapper2 :: (Pretty.Doc -> Pretty.Doc -> Pretty.Doc)
         -> CDoc        -> CDoc       -> CDoc
wrapper2 f d1 d2 = CDoc $ \sty -> f (runCDoc d1 sty) (runCDoc d2 sty)

parens, braces, brackets, quotes, doubleQuotes, angles :: CDoc -> CDoc
parens = wrapper Pretty.parens
braces = wrapper Pretty.braces
brackets = wrapper Pretty.brackets
doubleQuotes = wrapper Pretty.doubleQuotes
angles d = char '<' <> d <> char '>'

-- | Wrap a 'CDoc' in single quotes, but only if the 'CDoc' does not begin
-- or end with a single quote. This way we get foo' instead of `foo''.
quotes d = CDoc $ \sty ->
    let doc = runCDoc d sty
        str = show doc
    in case str of
        ""     -> prettyQuoteDoc doc
        '\'':_ -> doc
        _ -> case last str of
            '\'' -> doc
            _    -> prettyQuoteDoc doc

--------------------------------------------------
--     Common small docs
--------------------------------------------------

semi, comma, colon, dcolon, equals, space, underscore, dot, vbar, arrow              :: CDoc
larrow, darrow, lparen, rparen, lbrack, rbrack, lbrace, rbrace, backslash, blankLine :: CDoc
semi       = char ';'
comma      = char ','
colon      = char ':'
dcolon     = text "::"
equals     = char '='
space      = char ' '
underscore = char '_'
dot        = char '.'
vbar       = char '|'
arrow      = text "->"
darrow     = text "=>"
larrow     = text "<-"
lparen     = char '('
rparen     = char ')'
lbrack     = char '['
rbrack     = char ']'
lbrace     = char '{'
rbrace     = char '}'
backslash  = char '\\'
blankLine  = text ""

-- | Indent a 'CDoc' by some amount
nest :: Int -> CDoc -> CDoc
-- | Join two 'CDoc's with a space between them
(<+>) :: CDoc -> CDoc -> CDoc
-- | Join two 'CDoc's vertically. If they don't vertically overlap, they
-- are joined neatly onto one line.
($$) :: CDoc -> CDoc -> CDoc
-- | Join two 'CDoc's vertically
($+$) :: CDoc -> CDoc -> CDoc

nest n = wrapper (Pretty.nest n)
(<+>) = wrapper2 (Pretty.<+>)
($$)  = wrapper2 (Pretty.$$)
($+$) = wrapper2 (Pretty.$+$)

listWrapper :: ([Pretty.Doc] -> Pretty.Doc) -> [CDoc] -> CDoc
listWrapper f ds = CDoc $ \sty -> f [runCDoc d sty | d <- ds]

hcat, hsep, vcat, sep, cat, fsep, fcat :: [CDoc] -> CDoc
-- | Concatenate 'CDoc's horizontally, as if with <>
hcat = listWrapper Pretty.hcat
-- | Concatenate 'CDoc's horizontally, as if with <+>
hsep = listWrapper Pretty.hsep
-- | Concatenate 'CDoc's vertically, as if with $$
vcat = listWrapper Pretty.vcat
-- | Behaves like either 'hsep' or 'vcat', depending on what fits
sep  = listWrapper Pretty.sep
-- | Behaves like either 'hcat' or 'vcat', depending on what fits
cat  = listWrapper Pretty.cat
-- | "Paragraph fill." Behaves a lot like 'sep', but it keeps putting
-- things on one line until it can't fit any more.
fsep = listWrapper Pretty.fsep
-- | Behaves like 'fsep', but uses <> instead of <+>
fcat = listWrapper Pretty.fcat

punctuate :: CDoc -> [CDoc] -> [CDoc]
punctuate _ [] = []
punctuate p (d:ds) = go d ds
  where go d []     = [d]
        go d (e:es) = (d <> p) : go e es

pprString :: String -> CDoc
pprString = vcat . map text . lines

pprWhen :: Bool -> CDoc -> CDoc
pprWhen True d  = d
pprWhen False _ = mempty

pprUnless :: Bool -> CDoc -> CDoc
pprUnless b = pprWhen (not b)

parensIf :: Bool -> CDoc -> CDoc
parensIf True  = parens
parensIf False = id

prettyQuoteDoc :: Pretty.Doc -> Pretty.Doc
prettyQuoteDoc d = Pretty.char '`' <> d <> Pretty.char '\''

prettyQuote :: CDoc -> CDoc
prettyQuote = wrapper prettyQuoteDoc

pprWithCommas :: Outputable a => [a] -> CDoc
pprWithCommas = fsep . punctuate comma . map ppr

asPrefixVar :: CDoc -> CDoc
asPrefixVar d
  | isOperatorDoc d = parens d
  | otherwise       = d

asInfixVar :: CDoc -> CDoc
asInfixVar d
  | isOperatorDoc d = d
  | otherwise       = char '`' <> d <> char '`'

isOperatorDoc :: CDoc -> Bool
isOperatorDoc d = let str = output d in
    not (null str) && isSymbol (head str)

--------------------------------------------------------------------------------------
--
--                             RENDERING
--
--------------------------------------------------------------------------------------

-- from ghc compiler/Utils/Pretty.hs, which is Text.PrettyPrint but slightly modified
-- In ghc it is called printDoc_ and takes args in a slightly different order.
hPrintDoc_ :: Handle -> Pretty.Mode -> Int -> Pretty.Doc -> IO ()
hPrintDoc_ hdl mode pprCols doc = do
    Pretty.fullRender mode pprCols 1.5 put done doc
    hFlush hdl
        -- for performance reasons, GHC contains a modified copy of
        -- Text.PrettyPrint. One difference is that 'TextDetails' contains
        -- more cases, for fast strings, fast z strings, etc.
  where put (Pretty.Chr c)  next = hPutChar hdl c >> next
        put (Pretty.Str s)  next = hPutStr  hdl s >> next
        put (Pretty.PStr s) next = hPutStr  hdl s >> next

        done = pure ()

-- | Print a 'CDoc' to a handle. Clears the color if an exception is thrown during
-- printing, to try and avoid screwing up the terminal.
hPrintCDoc :: Handle -> Pretty.Mode -> Flags -> CDocStyle -> CDoc -> IO ()
hPrintCDoc hdl mode flags sty doc = hPrintDoc_ hdl mode 100 (runCDoc doc ctx)
    -- TODO: colors
    --`finally`
    --    hPrintDoc_ hdl mode cols (runCDoc (colored Color.colReset empty) ctx)
  where ctx = initCDocContext flags sty

-- | Like 'hPrintCDoc', but also appends a newline to the output.
hPrintCDocLn :: Handle -> Pretty.Mode -> Flags -> CDocStyle -> CDoc -> IO ()
hPrintCDocLn hdl mode flags sty doc = hPrintCDoc hdl mode flags sty (doc $$ blankLine)
                                      -- NB. $$ is a no-op if either arg is 'empty'

dummyContext :: CDocContext
dummyContext = CDocContext
    { cdocFlags = noFlags
    -- this context is typically for use in GHCi while debugging
    -- the compiler, if your terminal doesn't support colors, change it!
    , cdocStyle = UserStyle FullDepth Colored
    }

output :: Outputable a => a -> String
output = show . flip runCDoc dummyContext . ppr

outputWith :: Outputable a => Flags -> CDocStyle -> a -> String
outputWith flags style = show
                       . flip runCDoc (initCDocContext flags style)
                       . ppr

renderWithStyle :: Flags -> CDocStyle -> CDoc -> String
renderWithStyle flags sty doc =
    -- uses line length of 100
    Pretty.renderStyle Pretty.style $ runCDoc doc (initCDocContext flags sty)

--------------------------------------------------
-- Class and instances
--------------------------------------------------

class Outputable a where
    ppr :: a -> CDoc
    pprPrec :: Rational -> a -> CDoc

    ppr = pprPrec 0
    pprPrec _ = ppr

    pprList :: [a] -> CDoc
    pprList xs = brackets $ fsep $ punctuate comma $ map ppr xs
    {-# MINIMAL ppr | pprPrec #-}

instance Outputable CDoc where
    ppr = id

instance Outputable Char where
    ppr = char
    pprList = {-doubleQuotes .-} pprString

instance Outputable Bool where
    ppr = text . show

instance Outputable Ordering where
    ppr = text . show

instance Outputable Int where
    ppr = int

instance Outputable Integer where
    ppr = integer

instance Outputable Float where
    ppr = float

instance Outputable Double where
    ppr = double

instance Outputable () where
    ppr _ = text "()"

instance (Outputable a) => Outputable [a] where
    ppr = pprList

instance (Outputable a) => Outputable (Set.Set a) where
    ppr s = braces $ fsep $ punctuate comma $ map ppr $ Set.toList s

instance (Outputable a, Outputable b) => Outputable (a, b) where
    ppr (x, y) = parens $ sep [ppr x <> comma, ppr y]

instance Outputable a => Outputable (Maybe a) where
    ppr Nothing  = text "Nothing"
    ppr (Just x) = text "Just" <+> ppr x

instance (Outputable a, Outputable b) => Outputable (Either a b) where
    ppr (Left x)  = text "Left"  <+> ppr x
    ppr (Right y) = text "Right" <+> ppr y

instance (Outputable key, Outputable e) => Outputable (Map.Map key e) where
    ppr m = ppr $ Map.toList m

instance (Outputable a, Outputable b, Outputable c) => Outputable (a, b, c) where
    ppr (x,y,z) =
      parens (sep [ppr x <> comma,
                   ppr y <> comma,
                   ppr z ])

instance (Outputable a, Outputable b, Outputable c, Outputable d) =>
         Outputable (a, b, c, d) where
    ppr (a,b,c,d) =
      parens (sep [ppr a <> comma,
                   ppr b <> comma,
                   ppr c <> comma,
                   ppr d])

instance (Outputable a, Outputable b, Outputable c, Outputable d, Outputable e) =>
         Outputable (a, b, c, d, e) where
    ppr (a,b,c,d,e) =
      parens (sep [ppr a <> comma,
                   ppr b <> comma,
                   ppr c <> comma,
                   ppr d <> comma,
                   ppr e])

instance ( Outputable a, Outputable b, Outputable c
         , Outputable d, Outputable e, Outputable f) =>
         Outputable (a, b, c, d, e, f) where
    ppr (a,b,c,d,e,f) =
      parens (sep [ppr a <> comma,
                   ppr b <> comma,
                   ppr c <> comma,
                   ppr d <> comma,
                   ppr e <> comma,
                   ppr f])

instance ( Outputable a, Outputable b, Outputable c, Outputable d
         , Outputable e, Outputable f, Outputable g) =>
         Outputable (a, b, c, d, e, f, g) where
    ppr (a,b,c,d,e,f,g) =
      parens (sep [ppr a <> comma,
                   ppr b <> comma,
                   ppr c <> comma,
                   ppr d <> comma,
                   ppr e <> comma,
                   ppr f <> comma,
                   ppr g])

instance Outputable Text.Text where
    ppr = ppr . Text.unpack

instance Outputable Parsec.ParseError where
    ppr = text . show

-- We are not currently using MegaParsec.
instance ( MParsec.Stream s
         , MParsec.ShowErrorComponent e
         ) => Outputable (MParsec.ParseErrorBundle s e) where
    ppr = text . MParsec.errorBundlePretty
