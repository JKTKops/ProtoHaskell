{-
Pretty printing.

This module used to be mainly of my own design, but the 'CDoc' version
is instead a HEAVILY modified version of GHC's 'Outputable' module.

The two modules are not interchangeable, due to differences in
naming preferences and the content of 'Settings'.
-}
module Utils.Outputable
    ( CDoc -- abstract
    , CDocContext(..), initCDocContext
    , CDocStyle(..), Depth(..), Colored(..), CodeStyle(..)
    , mkCodeStyle
    , mkUserStyle, mkDumpStyle, mkInfoStyle, mkObjectStyle, mkMessageStyle

    -- * Configure printing for a 'CDoc'
    , setCDocStyle, cdocDeeper, cdocDeeperList, cdocSetDepth
    , withCDocSettings, updCDocSettings, setCDocSettings
    , withIsDebugStyle, ifDebugStyle, whenDebugStyle

    -- * Common small 'CDoc's
    , semi, comma, colon, dcolon, equals, space, underscore
    , dot, vbar, arrow, darrow, larrow, lparen, rparen, lbrack
    , rbrack, lbrace, rbrace, backslash, bullet, star, blankLine

    -- * Basic 'CDoc' construction
    , empty, char, string, text, int, integer, float, double

    -- * Common 'CDoc' combinators
    , parens, braces, brackets, quotes, doubleQuotes, angles
    , nest, indent, hang, (<+>), (<>), ($$), ($+$), align, hcat, hsep
    , vcat, sep, cat, fsep, fcat, punctuate

    , pprWhen, pprUnless, parensIf, pprWithCommas
    , asPrefixVar, asInfixVar

    -- * Coloring
    , black, red, green, yellow, blue, magenta, cyan, white
    , dullBlack, dullRed, dullGreen, dullYellow, dullBlue
    , dullMagenta, dullCyan, dullWhite

    -- * Rendering
--    , hPrintCDoc, hPrintCDocLn, printCDocBasic
    , putCDoc, putCDocLn

    -- * Outputable class
    , Outputable(..)
    , output, outputWith, outputM, outputWithM, --renderWithStyle
    ) where

import qualified Data.Text.Prettyprint.Doc as Pretty
import           Data.Text.Prettyprint.Doc.Render.Terminal
import Compiler.Settings

import Data.Char
import Data.Functor ((<&>))
import Data.String (IsString(..))

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

newtype CDoc = CDoc { runCDoc :: CDocContext -> PrettyDoc }

instance IsString CDoc where
    fromString = string

type PrettyDoc = Pretty.Doc AnsiStyle

data CDocContext = CDocContext
     { cdocStyle    :: !CDocStyle
     , cdocSettings :: !Settings
     -- coloring information coming soon
     }

initCDocContext :: Settings -> CDocStyle -> CDocContext
initCDocContext stgs style = CDocContext style stgs

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
-- | Haven't implemented coloring yet.
data Colored = Colored
             | Uncolored
data CodeStyle = CStyle    -- ^ Still don't know if we're targetting C or Java.
               | JavaStyle

isDebugStyle :: CDocStyle -> Bool
isDebugStyle DebugStyle = True
isDebugStyle _          = False

isCodeStyle :: CDocStyle -> Bool
isCodeStyle CodeStyle{} = True
isCodeStyle _           = False

codeStyle :: CDocStyle -> Maybe CodeStyle
codeStyle (CodeStyle cs) = Just cs
codeStyle _              = Nothing

mkCodeStyle :: CodeStyle -> CDocStyle
mkCodeStyle = CodeStyle

type StyleCreator = Settings -> CDocStyle

mkMessageStyle, mkDumpStyle, mkInfoStyle, mkObjectStyle :: StyleCreator
mkMessageStyle settings = mkUserStyle settings FullDepth
mkDumpStyle   _settings = DumpStyle
mkInfoStyle    settings = mkUserStyle settings FullDepth
mkObjectStyle _settings = CodeStyle $ error "Just pick a target language already :("

mkUserStyle :: Settings -> Depth -> CDocStyle
mkUserStyle settings depth
  | gOpt FPprDebug settings = DebugStyle
  | otherwise = UserStyle depth c
  where
    c | shouldUseColor settings = Colored
      | otherwise               = Uncolored

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
    CDocContext{cdocStyle = UserStyle (PartialDepth 0) _} -> Pretty.pretty ("..." :: String)
    CDocContext{cdocStyle = UserStyle (PartialDepth n) c} ->
        runCDoc d ctx{cdocStyle = UserStyle (PartialDepth (n - 1)) c}
    _ -> runCDoc d ctx

-- | Truncate a list of 'CDoc' if it is longer than the current depth.
cdocDeeperList :: ([CDoc] -> CDoc) -> [CDoc] -> CDoc
cdocDeeperList f ds
  | null ds   = f []
  | otherwise = CDoc worker
  where worker ctx@CDocContext{cdocStyle = UserStyle (PartialDepth n) c}
          | n == 0    = Pretty.pretty ("..." :: String)
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

withCDocSettings :: (Settings -> CDoc) -> CDoc
withCDocSettings f = CDoc $ \ctx -> runCDoc (f $ cdocSettings ctx) ctx

updCDocSettings :: (Settings -> Settings) -> CDoc -> CDoc
updCDocSettings upd d = CDoc $ \ctx ->
    runCDoc d (ctx{cdocSettings = upd (cdocSettings ctx)})

setCDocSettings :: Settings -> CDoc -> CDoc
setCDocSettings stgs = updCDocSettings (const stgs)

withIsDebugStyle :: (Bool -> CDoc) -> CDoc
withIsDebugStyle f = withCDocStyle $ f . isDebugStyle

ifDebugStyle :: CDoc -- ^ if we are in debug style
             -> CDoc -- ^ otherwise
             -> CDoc
ifDebugStyle yes no = withIsDebugStyle $ \b -> if b then yes else no

whenDebugStyle :: CDoc -> CDoc
whenDebugStyle d = ifDebugStyle d empty

withCodeStyle :: (Maybe CodeStyle -> CDoc) -> CDoc
withCodeStyle f = CDoc $ \ctx -> runCDoc (f $ codeStyle $ cdocStyle ctx) ctx

--------------------------------------------------------------------------------------
--
-- Basic means of constructing CDocs
--
--------------------------------------------------------------------------------------

doc2CDoc :: PrettyDoc -> CDoc
doc2CDoc = CDoc . const

empty    :: CDoc
char     :: Char      -> CDoc
string   :: String    -> CDoc
text     :: Text.Text -> CDoc
int      :: Int       -> CDoc
integer  :: Integer   -> CDoc
float    :: Float     -> CDoc
double   :: Double    -> CDoc

empty    = doc2CDoc   Pretty.emptyDoc
char     = doc2CDoc . Pretty.pretty
string   = doc2CDoc . Pretty.pretty
text     = doc2CDoc . Pretty.pretty
int      = doc2CDoc . Pretty.pretty
integer  = doc2CDoc . Pretty.pretty
float    = doc2CDoc . Pretty.pretty
double   = doc2CDoc . Pretty.pretty

-- The use of lambdas actually affects how GHC will inline these definitions,
-- and we need them to get inlined after receiving ONE argument.
-- See the GHC user guide for more information.
{- HLINT ignore "Redundant lambda" -}
wrapper :: (PrettyDoc -> PrettyDoc) -> CDoc -> CDoc
wrapper f = \d -> CDoc $ f . runCDoc d
{-# INLINE wrapper #-}

wrapper2 :: (PrettyDoc -> PrettyDoc -> PrettyDoc)
         -> CDoc       -> CDoc      -> CDoc
wrapper2 f = \d1 d2 -> CDoc $ \sty -> f (runCDoc d1 sty) (runCDoc d2 sty)
{-# INLINE wrapper2 #-}

parens, braces, brackets, quotes, doubleQuotes, angles :: CDoc -> CDoc
parens = wrapper Pretty.parens
braces = wrapper Pretty.braces
brackets = wrapper Pretty.brackets
doubleQuotes = wrapper Pretty.dquotes
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
bullet, star :: CDoc
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
bullet     = char '*'
star       = char '*'

forall :: CDoc
forall = text "forall"

{- NOTE: [Operator Fixities]
In Text.PrettyPrint, the operators are all infixl. In practice, the distinction is
not all that important for writing correct code. Presumably, the layout algorithm performs
better on left-nested constructions. To this end, our $$ and $+$ are infixl since they have
a different precedence. However, my biggest gripe with Text.PrettyPrint is that the <>
operator clashes with Prelude.<>, and since Prelude.<> is infixr, we must have <+>
be infixr to match.

Further note: We no longer use Text.PrettyPrint, but the fixity of <+> is still relevant
because <> is Semigroup.<>.
-}

{- | Increase the nesting level of following lines of text by a given amount.

> >>> nest 4 $ vsep ["lorem", "ipsum", "dolor"]
> lorem
>     ipsum
>     dolor

-}
nest :: Int -> CDoc -> CDoc
-- | Indent a doc by a specified amount.
indent :: Int -> CDoc -> CDoc
-- | Lay out the CDoc with the nesting level set as current column + n.
-- Distinct from 'nest', which uses current nesting level + n.
hang :: Int -> CDoc -> CDoc
-- | Join two 'CDoc's with a space between them. If either is empty, the
-- space is omitted.
--
-- Note that Text.PrettyPrint operators are infixl, but these are infixr.
(<+>) :: CDoc -> CDoc -> CDoc
-- | Join two 'CDoc's vertically.
-- Formerly, if they didn't vertically overlap, they
-- were joined neatly onto one line. This behavior was removed after the switch
-- to the prettyprinter library.
($$) :: CDoc -> CDoc -> CDoc
-- | Join two 'CDoc's vertically
($+$) :: CDoc -> CDoc -> CDoc

nest n = wrapper (Pretty.nest n)
indent n = wrapper (Pretty.indent n)
hang n = wrapper (Pretty.hang n)

-- prettyprinter's <+> combinator, for whatever reason, doesn't bother
-- to check if either side is EmptyDoc. This is less robust, but at least
-- it should work... basically always.
(<+>) = wrapper2 $ \left right ->
    if null (show left)
    then right
    else if null (show right)
         then left
         else left Pretty.<+> right
($$)  = ($+$)
($+$) = wrapper2 $ \x y -> Pretty.align (Pretty.vsep [x, y])

infixr 6 <+>
infixl 5 $$, $+$

align :: CDoc -> CDoc
align = wrapper Pretty.align

listWrapper :: ([PrettyDoc] -> PrettyDoc) -> [CDoc] -> CDoc
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
fsep = listWrapper Pretty.fillSep
-- | Behaves like 'fsep', but uses <> instead of <+>
fcat = listWrapper Pretty.fillCat

punctuate :: CDoc -> [CDoc] -> [CDoc]
punctuate _ [] = []
punctuate p (d:ds) = go d ds
  where go d []     = [d]
        go d (e:es) = (d <> p) : go e es

pprWhen :: Bool -> CDoc -> CDoc
pprWhen True d  = d
pprWhen False _ = mempty

pprUnless :: Bool -> CDoc -> CDoc
pprUnless b = pprWhen (not b)

parensIf :: Bool -> CDoc -> CDoc
parensIf True  = parens
parensIf False = id

prettyQuoteDoc :: PrettyDoc -> PrettyDoc
prettyQuoteDoc d = Pretty.pretty '`' <> d <> Pretty.pretty '\''

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
-- Coloring
--------------------------------------------------------------------------------------

ifUseColor :: (CDoc -> CDoc) -> CDoc -> CDoc
ifUseColor f doc = withCDocStyle $ \case
    UserStyle _ Colored -> f doc
    _otherwise          -> doc

coloredVivid, coloredDull :: Color -> CDoc -> CDoc
coloredVivid c = ifUseColor (wrapper $ Pretty.annotate $ color c)
coloredDull  c = ifUseColor (wrapper $ Pretty.annotate $ colorDull c)

black, red, green, yellow, blue, magenta, cyan, white :: CDoc -> CDoc
black   = coloredVivid Black
red     = coloredVivid Red
green   = coloredVivid Green
yellow  = coloredVivid Yellow
blue    = coloredVivid Blue
magenta = coloredVivid Magenta
cyan    = coloredVivid Cyan
white   = coloredVivid White

dullBlack, dullRed, dullGreen, dullYellow  :: CDoc -> CDoc
dullBlue, dullMagenta, dullCyan, dullWhite :: CDoc -> CDoc

dullBlack   = coloredDull Black
dullRed     = coloredDull Red
dullGreen   = coloredDull Green
dullYellow  = coloredDull Yellow
dullBlue    = coloredDull Blue
dullMagenta = coloredDull Magenta
dullCyan    = coloredDull Cyan
dullWhite   = coloredDull White

--------------------------------------------------------------------------------------
--
--                             RENDERING
--
--------------------------------------------------------------------------------------

-- TODO: Re-implement these with prettyprinter package

-- -- from ghc compiler/Utils/Pretty.hs, which is Text.PrettyPrint but slightly modified
-- -- In ghc it is called printDoc_ and takes args in a slightly different order.
-- hPrintDoc_ :: Handle -> Pretty.Mode -> Int -> Pretty.Doc -> IO ()
-- hPrintDoc_ hdl mode pprCols doc = do
--     Pretty.fullRender mode pprCols 1.5 put done doc
--     hFlush hdl
--         -- for performance reasons, GHC contains a modified copy of
--         -- Text.PrettyPrint. One difference is that 'TextDetails' contains
--         -- more cases, for fast strings, fast z strings, etc.
--   where put (Pretty.Chr c)  next = hPutChar hdl c >> next
--         put (Pretty.Str s)  next = hPutStr  hdl s >> next
--         put (Pretty.PStr s) next = hPutStr  hdl s >> next

--         done = pure ()

-- -- | Print a 'CDoc' to a handle. Clears the color if an exception is thrown during
-- -- printing, to try and avoid screwing up the terminal.
-- hPrintCDoc :: Handle -> Pretty.Mode -> Settings -> CDocStyle -> CDoc -> IO ()
-- hPrintCDoc hdl mode stgs sty doc = hPrintDoc_ hdl mode 100 (runCDoc doc ctx)
--     --`finally`
--     --    hPrintDoc_ hdl mode cols (runCDoc (colored Color.colReset empty) ctx)
--   where ctx = initCDocContext stgs sty

-- -- | Like 'hPrintCDoc', but also appends a newline to the output.
-- hPrintCDocLn :: Handle -> Pretty.Mode -> Settings -> CDocStyle -> CDoc -> IO ()
-- hPrintCDocLn hdl mode stgs sty doc = hPrintCDoc hdl mode stgs sty (doc $$ blankLine)

-- TODO: this definition leaves the terminal covered if an error occurs while printing
-- which is pretty common due to laziness.
putCDoc ::  CDoc -> IO ()
putCDoc doc = putDoc $ runCDoc doc dummyContext

putCDocLn :: CDoc -> IO ()
putCDocLn = putCDoc . ($$ empty)

-- this style is typically for use in GHCi while debugging
-- the compiler, if your terminal doesn't support colors, change it!
dummyStyle :: CDocStyle
dummyStyle = UserStyle FullDepth Colored

dummyContext :: CDocContext
dummyContext = CDocContext
    { cdocSettings = defaultSettings
    , cdocStyle    = dummyStyle
    }

-- | Intended for debugging in GHCi; uses the 'defaultSettings' and the style
-- @UserStyle FullDepth Colored@.
output :: Outputable a => a -> String
output = show . flip runCDoc dummyContext . ppr

-- | See 'output'.
instance Show CDoc where show = output

outputWith :: Outputable a => Settings -> CDocStyle -> a -> String
outputWith stgs style = show
                       . flip runCDoc (initCDocContext stgs style)
                       . ppr

outputM :: (Functor m, HasSettings m, Outputable a) => a -> m String
outputM = outputWithM dummyStyle . ppr

outputWithM :: (Functor m, HasSettings m, Outputable a) => CDocStyle -> a -> m String
outputWithM style thing = getSettings <&> \s -> outputWith s style thing

-- renderWithStyle :: Settings -> CDocStyle -> CDoc -> String
-- renderWithStyle stgs sty doc =
--     -- uses line length of 100
--     Pretty.renderStyle Pretty.style $ runCDoc doc (initCDocContext stgs sty)

--------------------------------------------------
-- Class and instances
--------------------------------------------------

class Outputable a where
    ppr :: a -> CDoc
    pprPrec :: Rational -> a -> CDoc

    ppr = pprPrec 0
    pprPrec _ = ppr

    pprList :: [a] -> CDoc
    pprList xs = brackets $ align $ fsep $ punctuate comma $ map ppr xs
    {-# MINIMAL ppr | pprPrec #-}

instance Outputable CDoc where
    ppr = id

instance Outputable Char where
    ppr = char
    pprList = string

instance Outputable Bool where
    ppr True  = text "True"
    ppr False = text "False"

instance Outputable Ordering where
    ppr LT = text "LT"
    ppr EQ = text "EQ"
    ppr GT = text "GT"

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
    ppr (Just x) = text "Just" <+> align (ppr x)

instance (Outputable a, Outputable b) => Outputable (Either a b) where
    ppr (Left x)  = text "Left"  <+> align (ppr x)
    ppr (Right y) = text "Right" <+> align (ppr y)

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
    ppr = text

instance Outputable Parsec.ParseError where
    ppr = string . show

-- We are not currently using MegaParsec.
instance ( MParsec.Stream s
         , MParsec.ShowErrorComponent e
         ) => Outputable (MParsec.ParseErrorBundle s e) where
    ppr = string . MParsec.errorBundlePretty
