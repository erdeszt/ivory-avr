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
    ( Def
    , IvoryInit(ival)
    , IvoryOrd((>=?))
    , Proc((:->))
    , Proxy(Proxy)
    , body
    , breakOut
    , call_
    , deref
    , false
    , local
    , proc
    , setBit
    , store
    , times
    , true
    )
import Ivory.HW (setReg, readReg, writeReg)
import Ivory.Stdlib.Control (unless, when)
import Ivory.Avr.Atmega328p.Registers (regBitsTCCR1B, regTCNT1, cs10)

import Panic (PanicFn)
import SafeIx (SafeIx, safeIx, toIvoryIx)

type MaxDelay = 1001
type MaxWaitCount = 10001

delayInit :: Def ('[] :-> ())
delayInit = proc "delay_init" $ body $ do
    setReg regBitsTCCR1B (setBit cs10)

-- TODO:
--   * Add documentation
--   * Replace magic constant
delayMS :: PanicFn -> Def ('[SafeIx MaxDelay] :-> ())
delayMS panic = proc "delay" $ \interval -> body $ do
    toIvoryIx interval `times` \_ -> do
        writeReg regTCNT1 0
        waited <- local (ival false)
        toIvoryIx maxLoopCount `times` \_ -> do
            counterValue <- readReg regTCNT1
            when (counterValue >=? 16000) $ do
                store waited true
                breakOut
        waited' <- deref waited
        unless waited' (call_ panic)
  where
    maxLoopCount :: SafeIx MaxWaitCount
    maxLoopCount = safeIx (Proxy @10000)