{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Delay where

import GHC.TypeNats

import Ivory.Language

import Ivory.HW ( setReg, readReg, writeReg )

import Ivory.Avr.Atmega328p.Registers

type MaxDelay = 1001

type SafeIx (bound :: Nat) (n :: Nat) = (KnownNat n, KnownNat bound, n <= bound)

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
    maxLoopCount :: Ix 100000
    maxLoopCount = 10000

-- NOTE: Doesn't work because can't include it in module
-- delayMS :: SafeIx MaxDelay d => Proxy d -> Def ('[] :-> ())
-- delayMS interval = proc "delay" $ body $ do
--     (toLoopBound @MaxDelay interval) `times` \_ -> do
--         writeReg regTCNT1 0
--         maxLoopCount `times` \_ -> do
--             counterValue <- readReg regTCNT1
--             ifte_ (counterValue >=? 16000) breakOut (return ())
--     retVoid
--   where
--     maxLoopCount :: Ix 100000
--     maxLoopCount = 10000