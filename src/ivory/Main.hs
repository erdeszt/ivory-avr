{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Ivory.Compile.C.CmdlineFrontend ( runCompiler, initialOpts, Opts(outDir) )
import Ivory.HW
import Ivory.Language
import Ivory.Language.Uint
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
    writeReg reg_UBRR0H bitrate_h
    writeReg reg_UBRR0L bitrate_l
    setReg reg_UCSR0B (setBit txen0)
    setReg reg_UCSR0C $ do
        setBit ucsz01
        setBit ucsz00
    call_ delayInit
    forever $ do
        writeReg reg_UDR0 72
        call_ delayMS 1000

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

