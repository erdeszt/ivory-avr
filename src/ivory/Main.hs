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

import Data.Char

import Ivory.Language.Syntax.AST (Expr)
import Ivory.Compile.C.CmdlineFrontend ( runCompiler, initialOpts, Opts(outDir) )
import Ivory.HW
import Ivory.HW.BitData
import Ivory.Language
import Ivory.Language.Uint
import Ivory.Stdlib.String
import GHC.TypeNats

import Blink ( blinkMain )
import Delay ( delayInit, delayMS )
import Ivory.Avr.Atmega328p.Registers

panic :: ( BitData controlReg
         , IvoryIOReg (BitDataRep controlReg)
         , BitData reg
         , IvoryIOReg (BitDataRep reg)
         ) => (BitDataReg controlReg, BitDataField controlReg Bit)
           -> (BitDataReg reg, BitDataField reg Bit)
           -> Def ('[] :-> ())
panic (controlReg, controlBit) (reg, bit) = proc "panic" $ body $ do
    setReg controlReg (setBit controlBit)
    setReg reg (setBit bit)
    forever (return ())

panicWithOnboardLed :: Def ('[] :-> ())
panicWithOnboardLed = panic (regBitsPORTB, portb5) (regBitsDDRB, ddb5)

newtype SafeIx (n :: Nat) = SafeIx { getSafeIx :: Ix n }

toSafeIx :: (KnownNat n, KnownNat ix, n <= (ix - 1)) => Proxy n -> SafeIx ix
toSafeIx proxy = SafeIx (fromInteger . toInteger $ natVal proxy)

testIx :: (KnownNat n, n <= 10) => Proxy n -> Ix 10
testIx p = fromInteger . toInteger $ natVal p

type MaxLoopCount = 10
type MaxDelay = 20

safeTimes :: forall bound n eff a. (KnownNat n, KnownNat bound, n <= (bound - 1)) => Proxy n -> (Ix bound -> Ivory (AllowBreak eff) a) -> Ivory eff ()
safeTimes proxy body = do
    let ix = fromInteger . toInteger $ natVal proxy
    times ix body

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
            call_ delayMS 1
        call_ delayMS 1000
    call_ panicWithOnboardLed
  where
    helloStore :: Init ('Array 6 ('Stored Uint8))
    helloStore = iarray (replicate 6 (ival 0))

mainModule :: Module
mainModule = package "firmware" $ do
    hw_moduledef
    incl delayInit
    incl delayMS
    incl serialTxMain
    incl panicWithOnboardLed

-- Blink the onboard led (pin13, ddrb 5) on the Arduino Uno/Nano as fast as possible
rapidBlink :: Def ('[] :-> ())
rapidBlink = proc "rapidBlink" $ body $ do
    setReg regBitsDDRB (setBit ddb5)
    forever $ do
        setReg regBitsPORTB (setBit portb5)
        setReg regBitsPORTB (clearBit portb5)



main :: IO ()
main = do
    runCompiler [mainModule] hw_artifacts options
    return ()
  where
    options = initialOpts
      { outDir = Just "build/ivory"
      }

