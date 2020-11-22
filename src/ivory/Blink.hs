{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}

module Blink where

import Ivory.HW ( setReg )
import Ivory.Language
import GHC.TypeNats ()

import Ivory.Avr.Atmega328p.Registers
import Delay ( delayInit, delayMS )
import SafeIx

blinkMain :: Def ('[] :-> ())
blinkMain = proc "main" $ body $ do
    call_ delayInit
    setReg regBitsDDRB $ do
        setBit ddb5
    forever $ do
        setReg regBitsPORTB $ do
            setBit portb5
        call_ delayMS (safeIx (Proxy @1000))
        setReg regBitsPORTB $ do
            clearBit portb5
        call_ delayMS (safeIx (Proxy @1000))
