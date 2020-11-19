{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Delay where

import GHC.TypeNats

import Ivory.Language
import Ivory.Language.Syntax.Names

import Ivory.HW ( setReg, readReg, writeReg )

import Ivory.Avr.Atmega328p.Registers
import Ivory.Language.Proxy
import Ivory.Language.Type
import Ivory.Language.Syntax (Type(TyIndex))
import Ivory.Language.Syntax.AST (Expr)
import Ivory.Language.Syntax (Expr(ExpVar))

type MaxDelay = 1001
type MaxWaitCount = 10001

type SafeIx (bound :: Nat) (n :: Nat) = (KnownNat n, KnownNat bound, n <= bound)

newtype SafeIxT (n :: Nat) = SafeIxT { getSafeIx :: Expr }

toIxT :: (KnownNat n) => SafeIxT n -> Ix n
toIxT = wrapExpr . getSafeIx

toSafeIx :: (KnownNat n, KnownNat ix, n <= (ix - 1)) => Proxy n -> SafeIxT ix
toSafeIx proxy = SafeIxT (fromInteger . toInteger $ natVal proxy)

instance (KnownNat n) => IvoryType (SafeIxT n) where
    ivoryType _ = TyIndex (toInteger (natVal (Proxy @n)))

instance (KnownNat n) => IvoryVar (SafeIxT n) where
    wrapVar = wrapExpr . ExpVar
    unwrapExpr = getSafeIx


instance (KnownNat n) => IvoryExpr (SafeIxT n) where
    wrapExpr e | 0 /= fromTypeNat (aNat :: NatType n) = SafeIxT e
               | otherwise = error "cannot have an index with width 0"

-- instance (KnownNat n) => IvoryVar (SafeIxT n) where
--   wrapVar    =  wrapExpr . ExpVar
--   unwrapExpr = getIx

delayInit :: Def ('[] :-> ())
delayInit = proc "delay_init" $ body $ do
    setReg regBitsTCCR1B (setBit cs10)
    retVoid

delayMS :: Def ('[Ix MaxDelay] :-> ())
delayMS = proc "delay" $ \interval -> body $ do
    interval `times` \_ -> do
        writeReg regTCNT1 0
        maxLoopCount `times` \_ -> do
            counterValue <- readReg regTCNT1
            ifte_ (counterValue >=? 16000) breakOut (return ())
    retVoid
  where
    maxLoopCount :: Ix MaxWaitCount
    maxLoopCount = toLoopBound @MaxWaitCount (Proxy @10000)

delayMS2 :: Def ('[SafeIxT MaxDelay] :-> ())
delayMS2 = proc "delay" $ \interval -> body $ do
    toIxT interval `times` \_ -> do
        writeReg regTCNT1 0
        maxLoopCount `times` \_ -> do
            counterValue <- readReg regTCNT1
            ifte_ (counterValue >=? 16000) breakOut (return ())
    retVoid
  where
    maxLoopCount :: Ix MaxWaitCount
    maxLoopCount = toLoopBound @MaxWaitCount (Proxy @10000)

toLoopBound :: forall bound n. (SafeIx bound n) => Proxy n -> Ix bound
toLoopBound = fromInteger . toInteger . natVal

delayMSSafe :: SafeIx MaxDelay d => Proxy d -> Def ('[] :-> ())
delayMSSafe interval =
    let intervalNat = toInteger (natVal interval) in
    proc ("delaySafe" ++ show intervalNat) $ body $ do
        call_ delayMS (toLoopBound @MaxDelay interval)