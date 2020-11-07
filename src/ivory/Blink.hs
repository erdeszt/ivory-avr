{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Blink where

import Ivory.HW ( setReg )
import Ivory.Language
import GHC.TypeNats ()

import Registers ( ddrb1, portb1, reg_DDRB, reg_PORTB )
import Delay ( delayInit, delayMS )

blinkMain :: Def ('[] :-> ())
blinkMain = proc "main" $ body $ do
    call_ delayInit
    setReg reg_DDRB $ do
        setBit ddrb1
    forever $ do
        setReg reg_PORTB $ do
            setBit portb1
        call_ delayMS 1000
        setReg reg_PORTB $ do
            clearBit portb1
        call_ delayMS 1000
    retVoid
