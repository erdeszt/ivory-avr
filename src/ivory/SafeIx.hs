{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module SafeIx
    ( SafeIx
    , toIvoryIx
    , safeIx
    ) where

import Ivory.Language (IvoryVar, IvoryType, Ix)
import Ivory.Language.Type
import Ivory.Language.Proxy
import Ivory.Language.Syntax (Type(TyIndex), Expr(ExpVar))

import GHC.TypeNats

newtype SafeIx (n :: Nat) = SafeIx { getSafeIx :: Expr }

safeIx :: (KnownNat ix, KnownNat n, n <= (ix - 1)) => Proxy n -> SafeIx ix
safeIx = SafeIx . fromInteger .toInteger .natVal

toIvoryIx :: (KnownNat n) => SafeIx n -> Ix n
toIvoryIx = wrapExpr . getSafeIx

instance (KnownNat n) => IvoryType (SafeIx n) where
    ivoryType _ = TyIndex (toInteger (natVal (Proxy @n)))

instance (KnownNat n) => IvoryVar (SafeIx n) where
    wrapVar = wrapExpr . ExpVar
    unwrapExpr = getSafeIx

instance (KnownNat n) => IvoryExpr (SafeIx n) where
    wrapExpr e | 0 /= fromTypeNat (aNat :: NatType n) = SafeIx e
               | otherwise = error "cannot have an index with width 0"