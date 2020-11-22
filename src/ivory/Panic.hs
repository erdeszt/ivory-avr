{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Panic where

import Ivory.Language
    ( BitData
    , BitDataField
    , BitDataRep
    , Def
    , Proc ((:->))
    , setBit
    , forever
    , body
    , proc
    )
import Ivory.HW (IvoryIOReg, setReg)
import Ivory.HW.BitData (BitDataReg)
import Ivory.Language.BitData.Bits (Bit)
import Ivory.Avr.Atmega328p.Registers
  ( regBitsDDRB
  , ddb5
  , regBitsPORTB
  , portb5
  )

type PanicFn = Def ('[] :-> ())

panicWithLed ::
         ( BitData controlReg
         , IvoryIOReg (BitDataRep controlReg)
         , BitData reg
         , IvoryIOReg (BitDataRep reg)
         ) => (BitDataReg controlReg, BitDataField controlReg Bit)
           -> (BitDataReg reg, BitDataField reg Bit)
           -> Def ('[] :-> ())
panicWithLed (controlReg, controlBit) (reg, bit) = proc "panic" $ body $ do
    setReg controlReg (setBit controlBit)
    setReg reg (setBit bit)
    forever (return ())

panicWithOnboardLed :: Def ('[] :-> ())
panicWithOnboardLed = panicWithLed (regBitsPORTB, portb5) (regBitsDDRB, ddb5)