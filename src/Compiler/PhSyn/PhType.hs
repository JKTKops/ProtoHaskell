module Compiler.PhSyn.PhType where

import Data.String

import Utils.Outputable

newtype TVar id = TV id
  deriving (Show, Eq, Ord)

instance IsString id => IsString (TVar id) where
    fromString = TV . fromString

instance Outputable id => Outputable (TVar id) where
    ppr (TV name) = ppr name

data Type id
     = TVar (TVar id)
     | TCon (TyCon id)
     | TApp (Type id) (Type id)
     | (Type id) :-> (Type id)
     | Kinded Kind (Type id)    -- Constructed during type inference
     deriving (Show, Eq, Ord)

infixr :->

data Kind
     = KStar
     | KArr Kind Kind
     | KConstraint
     deriving (Show, Eq, Ord)

data Scheme id = Forall [TVar id] (Qual id (Type id))

data TyCon id
     = UserTyCon { tyId :: id }
     | PrimTyCon { tyId :: id }
     deriving (Show, Eq, Ord)

instance IsString id => IsString (TyCon id) where
    fromString = UserTyCon . fromString

data Pred id = IsIn id (Type id) deriving (Show, Eq, Ord)

data Qual id t = [Pred id] :=> t

------------------------------------------------------------------------------------
-- Alpha Equivalence
------------------------------------------------------------------------------------

class Alpha a where
    aeq :: a -> a -> Bool

instance Alpha (TVar id) where
    aeq _ _ = True

instance Eq id => Alpha (Type id) where
    aeq (TVar _) (TVar _) = True
    aeq (TApp a b) (TApp c d) = aeq a c && aeq b d
    aeq (a :-> b) (c :-> d) = aeq a c && aeq b d
    aeq (TCon a) (TCon b) = a == b
    aeq _ _ = False

typeInt, typeBool :: Type String
typeInt  = TCon "Int"
typeBool = TCon "Bool"

instance Outputable id => Outputable (Type id) where
    pprPrec _ (TVar v) = ppr v
    pprPrec _ (TCon c) = ppr c
    pprPrec p (t1 :-> t2) = parensIf (p > 0)
                             $ pprPrec (p + 1) t1
                             <+> text "->"
                             <+> pprPrec p t2

instance Outputable Kind where
    pprPrec _ KStar = char '*'
    pprPrec _ KConstraint = text "Constraint"
    pprPrec p (KArr k1 k2) = parensIf (p > 0)
                              $ pprPrec (p + 1) k1
                              <+> text "->"
                              <+> pprPrec p k2

instance Outputable id => Outputable (TyCon id) where
    ppr = ppr . tyId
