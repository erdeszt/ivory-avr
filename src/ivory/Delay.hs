{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Delay where

import Ivory.Language

import Ivory.HW ( setReg, readReg, writeReg )

import Registers ( cs10, reg_TCCR1B, reg_TCNT1 )

delayInit :: Def ('[] :-> ())
delayInit = proc "delay_init" $ body $ do
    setReg reg_TCCR1B $ do
        setBit cs10
    retVoid

delayMS :: Def ('[Ix 10000] :-> ())
delayMS = proc "delay" $ \interval -> body $ do
    interval `times` \_ -> do
        writeReg reg_TCNT1 0
        maxLoopCount `times` \_ -> do
            counterValue <- readReg reg_TCNT1
            ifte_ (counterValue >=? 16000) breakOut (return ())
    retVoid
  where
    maxLoopCount :: Ix 100000
    maxLoopCount = 10000