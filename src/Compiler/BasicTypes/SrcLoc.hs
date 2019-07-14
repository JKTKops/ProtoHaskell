-- Lots of ideas directly from ghc/comopiler/basicTypes/SrcLoc.hs
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Compiler.BasicTypes.SrcLoc
    (
      -- * SrcLoc
      RealSrcLoc -- Abstract export
    , SrcLoc(..)

      -- * Constructing SrcLoc
    , mkSrcLoc, mkRealSrcLoc, mkUnhelpfulSrcLoc
    , noSrcLoc
    , generatedSrcLoc
    , interactiveSrcLoc

    , advanceSrcLoc

      -- * Unsafe deconstruct SrcLoc
    , unsafeLocFile
    , unsafeLocLine
    , unsafeLocCol

      -- * SrcSpan
    , RealSrcSpan -- Abstract export
    , SrcSpan(..)

      -- * Constructing SrcSpan
    , mkSrcSpan, mkRealSrcSpan, mkUnhelpfulSrcSpan
    , noSrcSpan
    , wiredInSrcSpan
    , interactiveSrcSpan

    , realSrcLocSpan
    , srcLocSpan
    , combineRealSrcSpans
    , combineSrcSpans

      -- * Deconstructing SrcSpan
    , srcSpanStart, srcSpanEnd
    , realSrcSpanStart, realSrcSpanEnd
    , srcSpanFileMaybe

      -- * Unsafe deconstruct SrcSpan
    , unsafeSpanFile
    , unsafeSpanStartLine, unsafeSpanStartCol
    , unsafeSpanEndLine, unsafeSpanEndCol

      -- * SrcSpan predicates
    , isGoodSrcSpan
    , isOneLineSpan
    , containsSpan

    , spans

      -- * Located
    , Located, RealLocated, GenLocated(..)

      -- * Constructing Located
    , noLoc

      -- * Deconstructing Located
    , getLoc, unLoc
    , getRealSrcSpan, unRealLoc

      -- * Modifying Located
    , mapLoc

      -- * Combining and comparing Located
    , eqLocated, cmpLocated
    , combineLocs, addCombinedLoc
    ) where

import Utils.Outputable

import Data.Text.Lazy (Text)
import Data.List (intercalate)
import Data.Bits

---------------------------------------------------------------------
-- Source location information
---------------------------------------------------------------------

-- line/column numbers start at 1, so ex. tabstops are 1/9/17/...
data RealSrcLoc = SrcLoc { srcLocFile :: !Text, line, col :: !Int }
  deriving (Eq, Ord)

data SrcLoc = RealSrcLoc !RealSrcLoc
            | UnhelpfulLoc !Text
  deriving (Eq, Ord, Show)

mkSrcLoc :: Text -> Int -> Int -> SrcLoc
mkSrcLoc name line col = RealSrcLoc $ SrcLoc name line col

mkRealSrcLoc :: Text -> Int -> Int -> RealSrcLoc
mkRealSrcLoc = SrcLoc

noSrcLoc, generatedSrcLoc, interactiveSrcLoc :: SrcLoc
noSrcLoc          = UnhelpfulLoc "<no location info>"
generatedSrcLoc   = UnhelpfulLoc "<compiler-generated code>"
interactiveSrcLoc = UnhelpfulLoc "<interactive>"

mkUnhelpfulSrcLoc :: Text -> SrcLoc
mkUnhelpfulSrcLoc = UnhelpfulLoc

advanceSrcLoc :: RealSrcLoc -> Char -> RealSrcLoc
advanceSrcLoc (SrcLoc f l _) '\n' = SrcLoc f (l + 1) 1
advanceSrcLoc (SrcLoc f l c) '\t' = SrcLoc f l (nextTabStop c)
  where nextTabStop c = ((((c - 1) `shiftR` 3) + 1) `shiftL` 3) + 1
advanceSrcLoc (SrcLoc f l c) _    = SrcLoc f l (c + 1)

-----------------------------------------------------------------------
-- Source span information
-----------------------------------------------------------------------

{- | A RealSrcSpan denotes boundaries of a location in a text file.
     The end position is one character after the end of the span, so
     (1,1)-(1,1) is a 0-length span, and (1,1)-(1,2) is a 1-length span.
-}
data RealSrcSpan = SrcSpan { srcSpanFile :: !Text
                           , spanStartLine, spanStartCol
                           , spanEndLine, spanEndCol :: !Int
                           }
  deriving Eq

data SrcSpan = RealSrcSpan !RealSrcSpan
             | UnhelpfulSpan !Text
  deriving (Eq, Ord, Show)

mkSrcSpan :: SrcLoc -> SrcLoc -> SrcSpan
mkSrcSpan (UnhelpfulLoc str) _ = UnhelpfulSpan str
mkSrcSpan _ (UnhelpfulLoc str) = UnhelpfulSpan str
mkSrcSpan (RealSrcLoc loc1) (RealSrcLoc loc2) =
    RealSrcSpan $ mkRealSrcSpan loc1 loc2

mkRealSrcSpan :: RealSrcLoc -> RealSrcLoc -> RealSrcSpan
mkRealSrcSpan (SrcLoc f sline scol) (SrcLoc _ eline ecol) = SrcSpan f sline scol eline ecol

noSrcSpan, wiredInSrcSpan, interactiveSrcSpan :: SrcSpan
noSrcSpan          = UnhelpfulSpan "<no location info>"
wiredInSrcSpan     = UnhelpfulSpan "<wired into compiler>"
interactiveSrcSpan = UnhelpfulSpan "<interactive>"

-- | Make an unhelpful 'SrcSpan' with no location info
mkUnhelpfulSrcSpan :: Text -> SrcSpan
mkUnhelpfulSrcSpan = UnhelpfulSpan

-- | Turn a RealSrcLoc into a RealSrcSpan covering one point
realSrcLocSpan :: RealSrcLoc -> RealSrcSpan
realSrcLocSpan (SrcLoc f line col) = SrcSpan f line col line col

-- | Turn a SrcLoc into a SrcSpan covering one point. Handles unhelpful case.
srcLocSpan :: SrcLoc -> SrcSpan
srcLocSpan (UnhelpfulLoc str) = UnhelpfulSpan str
srcLocSpan (RealSrcLoc loc)   = RealSrcSpan $ realSrcLocSpan loc

-- | Combines two 'RealSrcSpan' into one that spans all the characters
-- contained in both spans. Assumes they belong to the same file.
combineRealSrcSpans :: RealSrcSpan -> RealSrcSpan -> RealSrcSpan
combineRealSrcSpans span1 span2 =
    SrcSpan file startLine startCol endLine endCol
  where (startLine, startCol) = min (spanStartLine span1, spanStartCol span1)
                                    (spanStartLine span2, spanStartCol span2)
        (endLine, endCol)     = max (spanEndLine span1, spanEndCol span1)
                                    (spanEndLine span2, spanEndCol span2)
        file = srcSpanFile span1

-- | Combines two 'SrcSpan' into one that spans all the characters
-- contained in both spans. If either span is 'UnhelpfulSpan', the other
-- is returned. Returns an 'UnhelpfulSpan' if the files differ.
combineSrcSpans :: SrcSpan -> SrcSpan -> SrcSpan
combineSrcSpans (UnhelpfulSpan _) span = span
combineSrcSpans span (UnhelpfulSpan _) = span
combineSrcSpans (RealSrcSpan span1) (RealSrcSpan span2)
  | srcSpanFile span1 == srcSpanFile span2 =
        RealSrcSpan $ combineRealSrcSpans span1 span2
  | otherwise = UnhelpfulSpan "<combineSrcSpans: files differ>"

--------------------------------------------------------------------------
-- SrcSpan predicates
--------------------------------------------------------------------------

-- | Tests if the span is contained on one line
isOneLineRealSpan :: RealSrcSpan -> Bool
isOneLineRealSpan (SrcSpan _ line1 _ line2 _) = line1 == line2

-- | Tests if the span covers only a single point
isPointRealSpan :: RealSrcSpan -> Bool
isPointRealSpan (SrcSpan _ sline scol eline ecol) =
    sline == eline && scol == ecol

-- | Tests if a 'SrcSpan' is real
isGoodSrcSpan :: SrcSpan -> Bool
isGoodSrcSpan (RealSrcSpan _) = True
isGoodSrcSpan (UnhelpfulSpan _) = False

-- | Tests if a 'SrcSpan' covers only one line
isOneLineSpan :: SrcSpan -> Bool
isOneLineSpan (RealSrcSpan span) = isOneLineRealSpan span
isOneLineSpan (UnhelpfulSpan _) = False

-- | Tests if the first span contains the second span.
-- True when the spans are equal.
-- False when either span is unhelpful.
containsSpan :: SrcSpan -> SrcSpan -> Bool
containsSpan (UnhelpfulSpan _) _ = False
containsSpan _ (UnhelpfulSpan _) = False
containsSpan (RealSrcSpan span1) (RealSrcSpan span2) =
    (spanStartLine span1, spanStartCol span1)
       <= (spanStartLine span2, spanStartCol span2)
    && (spanEndLine span1, spanEndCol span1)
       >= (spanEndLine span2, spanEndCol span2)
    && (srcSpanFile span1 == srcSpanFile span2)

-- | Tests if the RealSrcSpan contains the given (line, col) point.
spans :: RealSrcSpan -> (Int, Int) -> Bool
spans span (l, c) = realSrcSpanStart span <= loc && loc <= realSrcSpanEnd span
  where loc = SrcLoc (srcSpanFile span) l c

-------------------------------------------------------------------------
-- Unsafe access functions
-------------------------------------------------------------------------

unsafeLocFile :: SrcLoc -> Text
unsafeLocFile (RealSrcLoc s) = srcLocFile s

unsafeLocLine :: SrcLoc -> Int
unsafeLocLine (RealSrcLoc s) = line s

unsafeLocCol :: SrcLoc -> Int
unsafeLocCol (RealSrcLoc s) = col s

unsafeSpanFile :: SrcSpan -> Text
unsafeSpanFile (RealSrcSpan s) = srcSpanFile s

unsafeSpanStartLine :: SrcSpan -> Int
unsafeSpanStartLine (RealSrcSpan s) = spanStartLine s

unsafeSpanStartCol :: SrcSpan -> Int
unsafeSpanStartCol (RealSrcSpan s) = spanStartCol s

unsafeSpanEndLine :: SrcSpan -> Int
unsafeSpanEndLine (RealSrcSpan s) = spanEndLine s

unsafeSpanEndCol :: SrcSpan -> Int
unsafeSpanEndCol (RealSrcSpan s) = spanEndCol s

--------------------------------------------------------------------------
-- Safe access functions for SrcSpan
--------------------------------------------------------------------------

-- | Returns the location at the start of the 'SrcSpan'
-- or an unhelpful loc if the span is unhelpful.
srcSpanStart :: SrcSpan -> SrcLoc
srcSpanStart (UnhelpfulSpan str) = UnhelpfulLoc str
srcSpanStart (RealSrcSpan span) = RealSrcLoc $ realSrcSpanStart span

-- | Returns the location at the end of the 'SrcSpan'
-- or an unhelpful loc if the span is unhelpful
srcSpanEnd :: SrcSpan -> SrcLoc
srcSpanEnd (UnhelpfulSpan str) = UnhelpfulLoc str
srcSpanEnd (RealSrcSpan span) = RealSrcLoc $ realSrcSpanEnd span

-- | Returns the location at the start of the 'RealSrcSpan'
realSrcSpanStart :: RealSrcSpan -> RealSrcLoc
realSrcSpanStart (SrcSpan f l c _ _) = SrcLoc f l c

-- | Returns the location at the end of the 'RealSrcSpan'
realSrcSpanEnd :: RealSrcSpan -> RealSrcLoc
realSrcSpanEnd (SrcSpan f _ _ l c) = SrcLoc f l c

-- | Get the filename from good 'SrcSpan's.
srcSpanFileMaybe :: SrcSpan -> Maybe Text
srcSpanFileMaybe (RealSrcSpan s)   = Just (srcSpanFile s)
srcSpanFileMaybe (UnhelpfulSpan _) = Nothing

-------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------

instance Show RealSrcLoc where
    show (SrcLoc filename r c) =
        unwords ["SrcLoc", show filename, show r, show c]

-- Compare first by start point, then by end point
instance Ord RealSrcSpan where
    a `compare` b =
        (realSrcSpanStart a, realSrcSpanStart b)
        `compare` (realSrcSpanEnd a, realSrcSpanEnd b)

instance Show RealSrcSpan where
    show span@(SrcSpan file sl sc el ec)
      | isPointRealSpan span
      = unwords ["SrcSpanPoint", show file, show sl, show sc]

      | isOneLineRealSpan span
      =  unwords [ "SrcSpanOneLine", show file
                        , show sl, show sc, show ec ]

      | otherwise
      = unwords $ [ "SrcSpanMultiLine", show file]
                       ++ map show [sl, sc, el, ec]

instance Outputable RealSrcSpan where
    ppr = pprUserRealSpan True

instance Outputable SrcSpan where
    ppr = pprUserSpan True

pprUserSpan :: Bool -> SrcSpan -> Doc
pprUserSpan _ (UnhelpfulSpan s) = ppr s
pprUserSpan showPath (RealSrcSpan s) = pprUserRealSpan showPath s

pprUserRealSpan :: Bool -> RealSrcSpan -> Doc
pprUserRealSpan showPath span@(SrcSpan file sline scol eline ecol)
  | isPointRealSpan span
  = hcat [ ppWhen showPath (ppr file <> colon)
         , int sline <> colon
         , int scol
         ]
  | isOneLineRealSpan span
  = hcat [ ppWhen showPath (ppr file <> colon)
         , int sline <> colon
         , int scol
         , ppUnless (ecol - scol <= 1) (char '-' <> int (ecol - 1))
         ]
  | otherwise
  = hcat [ ppWhen showPath (ppr file <> colon)
         , parens (int sline <> comma <> int scol)
         , char '-'
         , parens (int eline <> comma <> int ecol')
         ]
  where ecol' = if ecol == 0 then ecol else ecol - 1

------------------------------------------------------------------------------------
-- Attaching SrcSpans to other things
-- AKA "Locating" them
------------------------------------------------------------------------------------

data GenLocated l e = Located l e
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

type Located = GenLocated SrcSpan
type RealLocated = GenLocated RealSrcSpan

mapLoc :: (a -> b) -> GenLocated l a -> GenLocated l b
mapLoc = fmap

unLoc :: GenLocated l a -> a
unLoc (Located _ a) = a

getLoc :: GenLocated l a -> l
getLoc (Located l _) = l

getRealSrcSpan :: RealLocated a -> RealSrcSpan
getRealSrcSpan = getLoc

unRealLoc :: RealLocated a -> a
unRealLoc = unLoc

noLoc :: a -> Located a
noLoc = Located noSrcSpan

combineLocs :: Located a -> Located b -> SrcSpan
combineLocs a b = combineSrcSpans (getLoc a) (getLoc b)

addCombinedLoc :: Located a -> Located b -> c -> Located c
addCombinedLoc a b = Located (combineLocs a b)

-- Not a satisfactory general Eq instance
-- ingores the location and compares the located things.
eqLocated :: Eq a => Located a -> Located a -> Bool
eqLocated a b = unLoc a == unLoc b

-- Not a satisfactory general Ord instance
-- ignores the location and compares the located things.
cmpLocated :: Ord a => Located a -> Located a -> Ordering
cmpLocated a b = unLoc a `compare` unLoc b

-- Ideally we'd like to have some sort of "debug" mode here
-- that prints the location information
-- But this would require recreating the entire Doc type from scratch
-- to allow the internal build function to have access to a debug boolean
instance Outputable a => Outputable (GenLocated l a) where
    ppr (Located _ a) = ppr a

-- So instead we provide this alternative:
pprWithLoc :: (Outputable l, Outputable e) => GenLocated l e -> Doc
pprWithLoc (Located l e) = braces (ppr l) $$ ppr e
