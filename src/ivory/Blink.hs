{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Blink where

import Ivory.HW ( setReg )
import Ivory.Language
import GHC.TypeNats ()

import Registers ( ddb1, portb1, regBitsDDRB, regBitsPORTB )
import Delay ( delayInit, delayMS )

blinkMain :: Def ('[] :-> ())
blinkMain = proc "main" $ body $ do
    call_ delayInit
    setReg regBitsDDRB $ do
        setBit ddb1
    forever $ do
        setReg regBitsPORTB $ do
            setBit portb1
        call_ delayMS 1000
        setReg regBitsPORTB $ do
            clearBit portb1
        call_ delayMS 1000
