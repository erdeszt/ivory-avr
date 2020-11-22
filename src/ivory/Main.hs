{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

module Main where


import Ivory.Compile.C.CmdlineFrontend ( runCompiler, initialOpts, Opts(outDir) )
import Ivory.HW
import Ivory.Language
import Ivory.Language.Uint
import Ivory.Stdlib.String
import Ivory.Avr.Atmega328p.Registers
import Delay -- ( delayInit, delayMS , delayMSSafe, delayMS2, MaxDelay )
import Panic
import SafeIx

delay :: Def ('[SafeIx MaxDelay] ':-> ())
delay = delayMS panicWithOnboardLed

blinkMain :: Def ('[] :-> ())
blinkMain = proc "main" $ body $ do
    call_ delayInit
    setReg regBitsDDRB $ do
        setBit ddb5
    forever $ do
        setReg regBitsPORTB $ do
            setBit portb5
        call_ delay (safeIx (Proxy @1000))
        setReg regBitsPORTB $ do
            clearBit portb5
        call_ delay (safeIx (Proxy @1000))

serialTxMain :: Def ('[] :-> ())
serialTxMain = proc "main" $ body $ do
    -- TODO: Investigate if this is correct:
    -- let fcpu = 16000000
    -- let baud = 9600
    -- let bitrate = Uint16 ((fcpu / 16 / baud) - 1)
    let bitrate = Uint16 103 -- (CPU_FREQ(=16MHZ) / 16 / BAUD(9600)) - 1
    let bitrate_h = bitCast @Uint16 @Uint8 ((bitrate `iShiftR` 8) .& 0xFF)
    let bitrate_l = bitCast @Uint16 @Uint8 (bitrate .& 0xFF)
    writeReg regUBRR0H bitrate_h
    writeReg regUBRR0L bitrate_l
    setReg regBitsUCSR0B (setBit txen0)
    setReg regBitsUCSR0C $ do
        setBit udord0
        setBit ucpha0
    call_ delayInit
    stringStore <- local helloStore
    _ <- string_lit_array "Hello\n" stringStore
    forever $ do
        arrayMap $ \ix -> do
            currentChar <- deref (stringStore ! ix)
            writeReg regUDR0 currentChar
            -- TODO: Wait until UDR0 is 0
            call_ delay (safeIx (Proxy @1))
        call_ delay (safeIx (Proxy @1000))
    call_ panicWithOnboardLed
  where
    helloStore :: Init ('Array 6 ('Stored Uint8))
    helloStore = iarray (replicate 6 (ival 0))

mainModule :: Module
mainModule = package "firmware" $ do
    hw_moduledef
    incl delayInit
    incl delay
    incl serialTxMain
    incl panicWithOnboardLed

main :: IO ()
main = do
    runCompiler [mainModule] hw_artifacts options
    return ()
  where
    options = initialOpts
      { outDir = Just "build/ivory"
      }

