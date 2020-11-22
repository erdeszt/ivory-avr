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

import Ivory.Language

import Ivory.HW ( setReg, readReg, writeReg )

import Ivory.Avr.Atmega328p.Registers

import SafeIx

type MaxDelay = 1001
type MaxWaitCount = 10001

delayInit :: Def ('[] :-> ())
delayInit = proc "delay_init" $ body $ do
    setReg regBitsTCCR1B (setBit cs10)
    retVoid

delayMS :: Def ('[SafeIx MaxDelay] :-> ())
delayMS = proc "delay" $ \interval -> body $ do
    toIvoryIx interval `times` \_ -> do
        writeReg regTCNT1 0
        maxLoopCount `times` \_ -> do
            counterValue <- readReg regTCNT1
            ifte_ (counterValue >=? 16000) breakOut (return ())
    retVoid
  where
    maxLoopCount :: Ix MaxWaitCount
    maxLoopCount = 10000