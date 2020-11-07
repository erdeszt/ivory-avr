{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Registers where

import Ivory.Language ( ivory, Bit, Bits, Uint16, Uint8 )

import Ivory.HW ( mkBitDataReg, mkReg, BitDataReg, Reg )

-- TODO:
-- constexpr u8 SFR_OFFSET = __AVR_ARCH__ >= 100 ? 0x00 : 0x20;
-- Assumed to be 0x20 for now
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

bitdata UCSR0B :: Bits 8 = ucsr0b
    { rxcie0 :: Bit
    , txcie0 :: Bit
    , udrie0 :: Bit
    , rxen0  :: Bit
    , txen0  :: Bit
    , ucsz02 :: Bit
    , rxb80  :: Bit
    , txb80  :: Bit
    }

bitdata UCSR0C :: Bits 8 = ucsr0c
    { umsel01 :: Bit
    , umsel00 :: Bit
    , upm01   :: Bit
    , upm00   :: Bit
    , usbs0   :: Bit
    , ucsz01  :: Bit
    , ucsz00  :: Bit
    , ucpol0  :: Bit
    }

|]

reg_DDRB :: BitDataReg DDRB
reg_DDRB = mkBitDataReg (0x04 + 0x20)

reg_PORTB :: BitDataReg PORTB
reg_PORTB = mkBitDataReg (0x05 + 0x20)

reg_TCCR1A :: BitDataReg TCCR1A
reg_TCCR1A = mkBitDataReg 0x80

reg_TCCR1B :: BitDataReg TCCR1B
reg_TCCR1B = mkBitDataReg 0x81

reg_UCSR0B :: BitDataReg UCSR0B
reg_UCSR0B = mkBitDataReg 0xC1

reg_UCSR0C :: BitDataReg UCSR0C
reg_UCSR0C = mkBitDataReg 0xC2

reg_TCNT1 :: Reg Uint16
reg_TCNT1 = mkReg 0x84

reg_UBRR0H :: Reg Uint8
reg_UBRR0H = mkReg 0xC5

reg_UBRR0L :: Reg Uint8
reg_UBRR0L = mkReg 0xC4

reg_UDR0 :: Reg Uint8
reg_UDR0 = mkReg 0xC6