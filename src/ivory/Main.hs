{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Ivory.Compile.C.CmdlineFrontend
import Ivory.HW
import Ivory.Language
import GHC.TypeNats

import System.Directory --TMP

-- TODO:
-- constexpr u8 SFR_OFFSET = __AVR_ARCH__ >= 100 ? 0x00 : 0x20;
-- Assumed to be 0 for now
[ivory|

bitdata DDRB :: Bits 8 = ddrb
    { ddrb7 :: Bit
    , ddrb6 :: Bit
    , ddrb5 :: Bit
    , ddrb4 :: Bit
    , ddrb3 :: Bit
    , ddrb2 :: Bit
    , ddrb1 :: Bit
    , ddrb0 :: Bit
    }

bitdata PORTB :: Bits 8 = portb
    { portb7 :: Bit
    , portb6 :: Bit
    , portb5 :: Bit
    , portb4 :: Bit
    , portb3 :: Bit
    , portb2 :: Bit
    , portb1 :: Bit
    , portb0 :: Bit
    }

bitdata TCCR1A :: Bits 8 = tccr1a
    { com1a1 :: Bit
    , com1a0 :: Bit
    , com1b1 :: Bit
    , com1b0 :: Bit
    , _      :: Bit
    , _      :: Bit
    , wgm11  :: Bit
    , wgm10  :: Bit
    }

bitdata TCCR1B :: Bits 8 = tccr1b
    { icnc1 :: Bit
    , ices1 :: Bit
    , wgm13 :: Bit
    , _     :: Bit
    , wgm12 :: Bit
    , cs12  :: Bit
    , cs11  :: Bit
    , cs10  :: Bit
    }

|]

reg_TCNT1 :: Reg Uint16
reg_TCNT1 = mkReg 0x84

reg_TCCR1A :: BitDataReg TCCR1A
reg_TCCR1A = mkBitDataReg 0x80

reg_TCCR1B :: BitDataReg TCCR1B
reg_TCCR1B = mkBitDataReg 0x81

reg_DDRB :: BitDataReg DDRB
reg_DDRB = mkBitDataReg (0x04 + 0x20)

reg_PORTB :: BitDataReg PORTB
reg_PORTB = mkBitDataReg (0x05 + 0x20)

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

mainProc :: Def ('[] :-> ())
mainProc = proc "main" $ body $ do
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

mainModule :: Module
mainModule = package "firmware" $ do
    hw_moduledef
    incl delayInit
    incl delayMS
    incl mainProc

main :: IO ()
main = do
  runCompiler [mainModule] hw_artifacts options
  cwd <- getCurrentDirectory
  putStrLn cwd
  return ()
  where
    options = initialOpts
        { outDir = Just "build/ivory"
        }
