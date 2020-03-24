module Utils.Outputable
    ( Outputable(..)
    , module Text.PrettyPrint

    -- * Custom pretty-printing functions
    , pprWhen, pprUnless
    , parensIf
    , prettyQuote
    , pprWithCommas

    -- * The actual showing function
    , output
    ) where

import Text.PrettyPrint hiding ((<>), empty)

--------------------------------------------------
-- Imports for providing instances
--------------------------------------------------
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text.Lazy as Text
import qualified Text.Parsec as Parsec
import qualified Text.Megaparsec as MParsec

pprString :: String -> Doc
pprString = vcat . map text . lines

pprWhen :: Bool -> Doc -> Doc
pprWhen True d  = d
pprWhen False _ = mempty

pprUnless :: Bool -> Doc -> Doc
pprUnless b = pprWhen (not b)

parensIf :: Bool -> Doc -> Doc
parensIf True  = parens
parensIf False = id

prettyQuote :: Doc -> Doc
prettyQuote d = char '`' <> d <> char '\''

pprWithCommas :: Outputable a => [a] -> Doc
pprWithCommas = fsep . punctuate comma . map ppr

output :: Outputable a => a -> String
output = show . ppr

--------------------------------------------------
-- Class and instances
--------------------------------------------------

class Outputable a where
    ppr :: a -> Doc
    pprPrec :: Rational -> a -> Doc

    ppr = pprPrec 0
    pprPrec _ = ppr

    pprList :: [a] -> Doc
    pprList xs = brackets $ fsep $ punctuate comma $ map ppr xs
    {-# MINIMAL ppr | pprPrec #-}

instance Outputable Char where
    ppr = char
    pprList = doubleQuotes . pprString

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

instance ( MParsec.Stream s
         , MParsec.ShowErrorComponent e
         ) => Outputable (MParsec.ParseErrorBundle s e) where
    ppr = text . MParsec.errorBundlePretty
