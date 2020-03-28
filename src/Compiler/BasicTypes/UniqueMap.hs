{-
One future improvement for the compiler would be to replace the unique system with one
that uses plain Ints and a fast FFI generation scheme. Then these maps could be replaced
with simple IntMaps.

Map operations should be right-biased, meaning arguments in right hand arguments
shadow those in left hand arguments. This is the reverse of Data.Map, but is more
natural when you expect updates as opposed to inserts.
-}
module Compiler.BasicTypes.UniqueMap where

import qualified Data.Map as M
import Compiler.BasicTypes.Unique

import Utils.Outputable

newtype UniqueMap a = UM (M.Map Unique a)
  deriving (Eq, Functor)

instance Semigroup (UniqueMap e) where
    (<>) = plusUM

instance Monoid (UniqueMap e) where
    mempty = emptyUM

instance Outputable e => Outputable (UniqueMap e) where
    ppr = pprUniqueMap ppr

pprUniqueMap :: (e -> CDoc) -> UniqueMap e -> CDoc
pprUniqueMap pprE um =
    brackets $ fsep $ punctuate comma
        [ ppr uniq <+> text ":->" <+> pprE e
        | (uniq, e) <- toListUM um
        ]

--------------------------------------------------------------------------------------
--                    UniqueMap operations
--
--  Feel free to add more operations if you find you need them. Wrappers for other
--  Data.Map functions are low-hanging fruit.
--------------------------------------------------------------------------------------

-- e.x. useValueUnique addToUM :: HasUnique e => e -> UniqueMap e -> UniqueMap e
useValueUnique :: HasUnique e => (Unique -> e -> a) -> e -> a
useValueUnique f v = f (getUnique v) v

emptyUM :: UniqueMap e
emptyUM = UM M.empty

isNullUM :: UniqueMap e -> Bool
isNullUM (UM m) = M.null m

singletonUM :: HasUnique key => key -> e -> UniqueMap e
singletonUM k v = UM (M.singleton (getUnique k) v)

directSingletonUM :: Unique -> e -> UniqueMap e
directSingletonUM u v = UM (M.singleton u v)

valueSingletonUM :: HasUnique e => e -> UniqueMap e
valueSingletonUM = useValueUnique directSingletonUM

listToUM :: HasUnique key => [(key, e)] -> UniqueMap e
listToUM pairs = directListToUM [ (getUnique k, v) | (k, v) <- pairs ]

directListToUM :: [(Unique, e)] -> UniqueMap e
directListToUM = UM . M.fromList

valueListToUM :: HasUnique e => [e] -> UniqueMap e
valueListToUM vs = directListToUM [ (getUnique v, v) | v <- vs ]

toListUM :: UniqueMap e -> [(Unique, e)]
toListUM (UM m) = M.toList m

addToUM :: HasUnique key => key -> e -> UniqueMap e -> UniqueMap e
addToUM k v (UM m) = UM $ M.insert (getUnique k) v m

directAddToUM :: Unique -> e -> UniqueMap e -> UniqueMap e
directAddToUM u v (UM m) = UM $ M.insert u v m

valueAddToUM :: HasUnique e => e -> UniqueMap e -> UniqueMap e
valueAddToUM = useValueUnique directAddToUM

deleteFromUM :: HasUnique key => key -> UniqueMap e -> UniqueMap e
deleteFromUM k (UM m) = UM $ M.delete (getUnique k) m

directDeleteFromUM :: Unique -> UniqueMap e -> UniqueMap e
directDeleteFromUM u (UM m) = UM $ M.delete u m

valueDeleteFromUM :: HasUnique e => e -> UniqueMap e -> UniqueMap e
valueDeleteFromUM = deleteFromUM

adjustUM :: HasUnique key => (e -> e) -> key -> UniqueMap e -> UniqueMap e
adjustUM f k (UM m) = UM $ M.adjust f (getUnique k) m

alterUM :: HasUnique key => (Maybe e -> Maybe e) -> key -> UniqueMap e -> UniqueMap e
alterUM f k (UM m) = UM $ M.alter f (getUnique k) m

-- | Right-biased map union
plusUM :: UniqueMap e -> UniqueMap e -> UniqueMap e
plusUM (UM x) (UM y) = UM $ M.union y x
