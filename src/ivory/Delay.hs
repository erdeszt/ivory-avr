{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Delay where

import Ivory.Language

import Ivory.HW ( setReg, readReg, writeReg )

import Ivory.Avr.Atmega328p.Registers

delayInit :: Def ('[] :-> ())
delayInit = proc "delay_init" $ body $ do
    setReg regBitsTCCR1B (setBit cs10)
    retVoid

delayMS :: Def ('[Ix 10000] :-> ())
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