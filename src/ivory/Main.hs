{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Char

import Ivory.Compile.C.CmdlineFrontend ( runCompiler, initialOpts, Opts(outDir) )
import Ivory.HW
import Ivory.Language
import Ivory.Language.Uint
import Ivory.Stdlib.String
import GHC.TypeNats ()

import Blink ( blinkMain )
import Delay ( delayInit, delayMS )
import Registers

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
    forever $ do
        arrayMap $ \ix -> do
            currentChar <- deref (stringStore ! ix)
            writeReg regUDR0 currentChar
            -- TODO: Wait until UDR0 is 0
            call_ delayMS 1
        call_ delayMS 1000
  where
    -- TODO: Fix the conversion so it doesn't hurt the eye
    helloStore :: Init ('Array 6 ('Stored Uint8))
    helloStore = iarray (map (ival . fromInteger . toInteger . ord) "Hello\n")

mainModule :: Module
mainModule = package "firmware" $ do
    hw_moduledef
    incl delayInit
    incl delayMS
    incl serialTxMain


main :: IO ()
main = do
    runCompiler [mainModule] hw_artifacts options
    return ()
  where
    options = initialOpts
      { outDir = Just "build/ivory"
      }

