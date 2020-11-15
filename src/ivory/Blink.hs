{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Blink where

import Ivory.HW ( setReg )
import Ivory.Language
import GHC.TypeNats ()

import Registers
import Delay ( delayInit, delayMS )

blinkMain :: Def ('[] :-> ())
blinkMain = proc "main" $ body $ do
    call_ delayInit
    setReg regBitsDDRB $ do
        setBit ddb5
    forever $ do
        setReg regBitsPORTB $ do
            setBit portb5
        call_ delayMS 1000
        setReg regBitsPORTB $ do
            clearBit portb5
        call_ delayMS 1000
