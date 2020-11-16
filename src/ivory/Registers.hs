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
-- Create package because takes forever to reload
[ivory|
bitdata PINB :: Bits 8 = pinb
  { pinb7 :: Bit
  , pinb6 :: Bit
  , pinb5 :: Bit
  , pinb4 :: Bit
  , pinb3 :: Bit
  , pinb2 :: Bit
  , pinb1 :: Bit
  , pinb0 :: Bit
  }
|]

regBitsPINB :: BitDataReg PINB
regBitsPINB = mkBitDataReg ( 32 + 3 )

regPINB :: Reg Uint8
regPINB = mkReg ( 32 + 3 )

[ivory|
bitdata DDRB :: Bits 8 = ddrb
  { ddb7 :: Bit
  , ddb6 :: Bit
  , ddb5 :: Bit
  , ddb4 :: Bit
  , ddb3 :: Bit
  , ddb2 :: Bit
  , ddb1 :: Bit
  , ddb0 :: Bit
  }
|]

regBitsDDRB :: BitDataReg DDRB
regBitsDDRB = mkBitDataReg ( 32 + 4 )

regDDRB :: Reg Uint8
regDDRB = mkReg ( 32 + 4 )

[ivory|
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
|]

regBitsPORTB :: BitDataReg PORTB
regBitsPORTB = mkBitDataReg ( 32 + 5 )

regPORTB :: Reg Uint8
regPORTB = mkReg ( 32 + 5 )

[ivory|
bitdata PINC :: Bits 8 = pinc
  { _     :: Bit
  , pinc6 :: Bit
  , pinc5 :: Bit
  , pinc4 :: Bit
  , pinc3 :: Bit
  , pinc2 :: Bit
  , pinc1 :: Bit
  , pinc0 :: Bit
  }
|]

regBitsPINC :: BitDataReg PINC
regBitsPINC = mkBitDataReg ( 32 + 6 )

regPINC :: Reg Uint8
regPINC = mkReg ( 32 + 6 )

[ivory|
bitdata DDRC :: Bits 8 = ddrc
  { _    :: Bit
  , ddc6 :: Bit
  , ddc5 :: Bit
  , ddc4 :: Bit
  , ddc3 :: Bit
  , ddc2 :: Bit
  , ddc1 :: Bit
  , ddc0 :: Bit
  }
|]

regBitsDDRC :: BitDataReg DDRC
regBitsDDRC = mkBitDataReg ( 32 + 7 )

regDDRC :: Reg Uint8
regDDRC = mkReg ( 32 + 7 )

[ivory|
bitdata PORTC :: Bits 8 = portc
  { _      :: Bit
  , portc6 :: Bit
  , portc5 :: Bit
  , portc4 :: Bit
  , portc3 :: Bit
  , portc2 :: Bit
  , portc1 :: Bit
  , portc0 :: Bit
  }
|]

regBitsPORTC :: BitDataReg PORTC
regBitsPORTC = mkBitDataReg ( 32 + 8 )

regPORTC :: Reg Uint8
regPORTC = mkReg ( 32 + 8 )

[ivory|
bitdata PIND :: Bits 8 = pind
  { pind7 :: Bit
  , pind6 :: Bit
  , pind5 :: Bit
  , pind4 :: Bit
  , pind3 :: Bit
  , pind2 :: Bit
  , pind1 :: Bit
  , pind0 :: Bit
  }
|]

regBitsPIND :: BitDataReg PIND
regBitsPIND = mkBitDataReg ( 32 + 9 )

regPIND :: Reg Uint8
regPIND = mkReg ( 32 + 9 )

[ivory|
bitdata DDRD :: Bits 8 = ddrd
  { ddd7 :: Bit
  , ddd6 :: Bit
  , ddd5 :: Bit
  , ddd4 :: Bit
  , ddd3 :: Bit
  , ddd2 :: Bit
  , ddd1 :: Bit
  , ddd0 :: Bit
  }
|]

regBitsDDRD :: BitDataReg DDRD
regBitsDDRD = mkBitDataReg ( 32 + 10 )

regDDRD :: Reg Uint8
regDDRD = mkReg ( 32 + 10 )

[ivory|
bitdata PORTD :: Bits 8 = portd
  { portd7 :: Bit
  , portd6 :: Bit
  , portd5 :: Bit
  , portd4 :: Bit
  , portd3 :: Bit
  , portd2 :: Bit
  , portd1 :: Bit
  , portd0 :: Bit
  }
|]

regBitsPORTD :: BitDataReg PORTD
regBitsPORTD = mkBitDataReg ( 32 + 11 )

regPORTD :: Reg Uint8
regPORTD = mkReg ( 32 + 11 )

[ivory|
bitdata TIFR0 :: Bits 8 = tifr0
  { _     :: Bit
  , _     :: Bit
  , _     :: Bit
  , _     :: Bit
  , _     :: Bit
  , ocf0b :: Bit
  , ocf0a :: Bit
  , tov0  :: Bit
  }
|]

regBitsTIFR0 :: BitDataReg TIFR0
regBitsTIFR0 = mkBitDataReg ( 32 + 21 )

regTIFR0 :: Reg Uint8
regTIFR0 = mkReg ( 32 + 21 )

[ivory|
bitdata TIFR1 :: Bits 8 = tifr1
  { _     :: Bit
  , _     :: Bit
  , icf1  :: Bit
  , _     :: Bit
  , _     :: Bit
  , ocf1b :: Bit
  , ocf1a :: Bit
  , tov1  :: Bit
  }
|]

regBitsTIFR1 :: BitDataReg TIFR1
regBitsTIFR1 = mkBitDataReg ( 32 + 22 )

regTIFR1 :: Reg Uint8
regTIFR1 = mkReg ( 32 + 22 )

[ivory|
bitdata TIFR2 :: Bits 8 = tifr2
  { _     :: Bit
  , _     :: Bit
  , _     :: Bit
  , _     :: Bit
  , _     :: Bit
  , ocf2b :: Bit
  , ocf2a :: Bit
  , tov2  :: Bit
  }
|]

regBitsTIFR2 :: BitDataReg TIFR2
regBitsTIFR2 = mkBitDataReg ( 32 + 23 )

regTIFR2 :: Reg Uint8
regTIFR2 = mkReg ( 32 + 23 )

[ivory|
bitdata PCIFR :: Bits 8 = pcifr
  { _     :: Bit
  , _     :: Bit
  , _     :: Bit
  , _     :: Bit
  , _     :: Bit
  , pcif2 :: Bit
  , pcif1 :: Bit
  , pcif0 :: Bit
  }
|]

regBitsPCIFR :: BitDataReg PCIFR
regBitsPCIFR = mkBitDataReg ( 32 + 27 )

regPCIFR :: Reg Uint8
regPCIFR = mkReg ( 32 + 27 )

[ivory|
bitdata EIFR :: Bits 8 = eifr
  { _     :: Bit
  , _     :: Bit
  , _     :: Bit
  , _     :: Bit
  , _     :: Bit
  , _     :: Bit
  , intf1 :: Bit
  , intf0 :: Bit
  }
|]

regBitsEIFR :: BitDataReg EIFR
regBitsEIFR = mkBitDataReg ( 32 + 28 )

regEIFR :: Reg Uint8
regEIFR = mkReg ( 32 + 28 )

[ivory|
bitdata EIMSK :: Bits 8 = eimsk
  { _    :: Bit
  , _    :: Bit
  , _    :: Bit
  , _    :: Bit
  , _    :: Bit
  , _    :: Bit
  , int1 :: Bit
  , int0 :: Bit
  }
|]

regBitsEIMSK :: BitDataReg EIMSK
regBitsEIMSK = mkBitDataReg ( 32 + 29 )

regEIMSK :: Reg Uint8
regEIMSK = mkReg ( 32 + 29 )

[ivory|
bitdata GPIOR0 :: Bits 8 = gpior0
  { gpior07 :: Bit
  , gpior06 :: Bit
  , gpior05 :: Bit
  , gpior04 :: Bit
  , gpior03 :: Bit
  , gpior02 :: Bit
  , gpior01 :: Bit
  , gpior00 :: Bit
  }
|]

regBitsGPIOR0 :: BitDataReg GPIOR0
regBitsGPIOR0 = mkBitDataReg ( 32 + 30 )

regGPIOR0 :: Reg Uint8
regGPIOR0 = mkReg ( 32 + 30 )

[ivory|
bitdata EECR :: Bits 8 = eecr
  { _     :: Bit
  , _     :: Bit
  , eepm1 :: Bit
  , eepm0 :: Bit
  , eerie :: Bit
  , eempe :: Bit
  , eepe  :: Bit
  , eere  :: Bit
  }
|]

regBitsEECR :: BitDataReg EECR
regBitsEECR = mkBitDataReg ( 32 + 31 )

regEECR :: Reg Uint8
regEECR = mkReg ( 32 + 31 )

[ivory|
bitdata EEDR :: Bits 8 = eedr
  { eedr7 :: Bit
  , eedr6 :: Bit
  , eedr5 :: Bit
  , eedr4 :: Bit
  , eedr3 :: Bit
  , eedr2 :: Bit
  , eedr1 :: Bit
  , eedr0 :: Bit
  }
|]

regBitsEEDR :: BitDataReg EEDR
regBitsEEDR = mkBitDataReg ( 32 + 32 )

regEEDR :: Reg Uint8
regEEDR = mkReg ( 32 + 32 )

[ivory|
bitdata EEAR :: Bits 16 = eear
  { _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  }
|]

regBitsEEAR :: BitDataReg EEAR
regBitsEEAR = mkBitDataReg ( 32 + 33 )

regEEAR :: Reg Uint16
regEEAR = mkReg ( 32 + 33 )

[ivory|
bitdata EEARL :: Bits 8 = eearl
  { eear7 :: Bit
  , eear6 :: Bit
  , eear5 :: Bit
  , eear4 :: Bit
  , eear3 :: Bit
  , eear2 :: Bit
  , eear1 :: Bit
  , eear0 :: Bit
  }
|]

regBitsEEARL :: BitDataReg EEARL
regBitsEEARL = mkBitDataReg ( 32 + 33 )

regEEARL :: Reg Uint8
regEEARL = mkReg ( 32 + 33 )

[ivory|
bitdata EEARH :: Bits 8 = eearh
  { _     :: Bit
  , _     :: Bit
  , _     :: Bit
  , _     :: Bit
  , _     :: Bit
  , _     :: Bit
  , eear9 :: Bit
  , eear8 :: Bit
  }
|]

regBitsEEARH :: BitDataReg EEARH
regBitsEEARH = mkBitDataReg ( 32 + 34 )

regEEARH :: Reg Uint8
regEEARH = mkReg ( 32 + 34 )

[ivory|
bitdata GTCCR :: Bits 8 = gtccr
  { tsm     :: Bit
  , _       :: Bit
  , _       :: Bit
  , _       :: Bit
  , _       :: Bit
  , _       :: Bit
  , psrasy  :: Bit
  , psrsync :: Bit
  }
|]

regBitsGTCCR :: BitDataReg GTCCR
regBitsGTCCR = mkBitDataReg ( 32 + 35 )

regGTCCR :: Reg Uint8
regGTCCR = mkReg ( 32 + 35 )

[ivory|
bitdata TCCR0A :: Bits 8 = tccr0a
  { com0a1 :: Bit
  , com0a0 :: Bit
  , com0b1 :: Bit
  , com0b0 :: Bit
  , _      :: Bit
  , _      :: Bit
  , wgm01  :: Bit
  , wgm00  :: Bit
  }
|]

regBitsTCCR0A :: BitDataReg TCCR0A
regBitsTCCR0A = mkBitDataReg ( 32 + 36 )

regTCCR0A :: Reg Uint8
regTCCR0A = mkReg ( 32 + 36 )

[ivory|
bitdata TCCR0B :: Bits 8 = tccr0b
  { foc0a :: Bit
  , foc0b :: Bit
  , _     :: Bit
  , _     :: Bit
  , wgm02 :: Bit
  , cs02  :: Bit
  , cs01  :: Bit
  , cs00  :: Bit
  }
|]

regBitsTCCR0B :: BitDataReg TCCR0B
regBitsTCCR0B = mkBitDataReg ( 32 + 37 )

regTCCR0B :: Reg Uint8
regTCCR0B = mkReg ( 32 + 37 )

[ivory|
bitdata TCNT0 :: Bits 8 = tcnt0
  { tcnt0_7 :: Bit
  , tcnt0_6 :: Bit
  , tcnt0_5 :: Bit
  , tcnt0_4 :: Bit
  , tcnt0_3 :: Bit
  , tcnt0_2 :: Bit
  , tcnt0_1 :: Bit
  , tcnt0_0 :: Bit
  }
|]

regBitsTCNT0 :: BitDataReg TCNT0
regBitsTCNT0 = mkBitDataReg ( 32 + 38 )

regTCNT0 :: Reg Uint8
regTCNT0 = mkReg ( 32 + 38 )

[ivory|
bitdata OCR0A :: Bits 8 = ocr0a
  { ocr0a_7 :: Bit
  , ocr0a_6 :: Bit
  , ocr0a_5 :: Bit
  , ocr0a_4 :: Bit
  , ocr0a_3 :: Bit
  , ocr0a_2 :: Bit
  , ocr0a_1 :: Bit
  , ocr0a_0 :: Bit
  }
|]

regBitsOCR0A :: BitDataReg OCR0A
regBitsOCR0A = mkBitDataReg ( 32 + 39 )

regOCR0A :: Reg Uint8
regOCR0A = mkReg ( 32 + 39 )

[ivory|
bitdata OCR0B :: Bits 8 = ocr0b
  { ocr0b_7 :: Bit
  , ocr0b_6 :: Bit
  , ocr0b_5 :: Bit
  , ocr0b_4 :: Bit
  , ocr0b_3 :: Bit
  , ocr0b_2 :: Bit
  , ocr0b_1 :: Bit
  , ocr0b_0 :: Bit
  }
|]

regBitsOCR0B :: BitDataReg OCR0B
regBitsOCR0B = mkBitDataReg ( 32 + 40 )

regOCR0B :: Reg Uint8
regOCR0B = mkReg ( 32 + 40 )

[ivory|
bitdata GPIOR1 :: Bits 8 = gpior1
  { gpior17 :: Bit
  , gpior16 :: Bit
  , gpior15 :: Bit
  , gpior14 :: Bit
  , gpior13 :: Bit
  , gpior12 :: Bit
  , gpior11 :: Bit
  , gpior10 :: Bit
  }
|]

regBitsGPIOR1 :: BitDataReg GPIOR1
regBitsGPIOR1 = mkBitDataReg ( 32 + 42 )

regGPIOR1 :: Reg Uint8
regGPIOR1 = mkReg ( 32 + 42 )

[ivory|
bitdata GPIOR2 :: Bits 8 = gpior2
  { gpior27 :: Bit
  , gpior26 :: Bit
  , gpior25 :: Bit
  , gpior24 :: Bit
  , gpior23 :: Bit
  , gpior22 :: Bit
  , gpior21 :: Bit
  , gpior20 :: Bit
  }
|]

regBitsGPIOR2 :: BitDataReg GPIOR2
regBitsGPIOR2 = mkBitDataReg ( 32 + 43 )

regGPIOR2 :: Reg Uint8
regGPIOR2 = mkReg ( 32 + 43 )

[ivory|
bitdata SPCR :: Bits 8 = spcr
  { spie :: Bit
  , spe  :: Bit
  , dord :: Bit
  , mstr :: Bit
  , cpol :: Bit
  , cpha :: Bit
  , spr1 :: Bit
  , spr0 :: Bit
  }
|]

regBitsSPCR :: BitDataReg SPCR
regBitsSPCR = mkBitDataReg ( 32 + 44 )

regSPCR :: Reg Uint8
regSPCR = mkReg ( 32 + 44 )

[ivory|
bitdata SPSR :: Bits 8 = spsr
  { spif  :: Bit
  , wcol  :: Bit
  , _     :: Bit
  , _     :: Bit
  , _     :: Bit
  , _     :: Bit
  , _     :: Bit
  , spi2x :: Bit
  }
|]

regBitsSPSR :: BitDataReg SPSR
regBitsSPSR = mkBitDataReg ( 32 + 45 )

regSPSR :: Reg Uint8
regSPSR = mkReg ( 32 + 45 )

[ivory|
bitdata SPDR :: Bits 8 = spdr
  { spdr7 :: Bit
  , spdr6 :: Bit
  , spdr5 :: Bit
  , spdr4 :: Bit
  , spdr3 :: Bit
  , spdr2 :: Bit
  , spdr1 :: Bit
  , spdr0 :: Bit
  }
|]

regBitsSPDR :: BitDataReg SPDR
regBitsSPDR = mkBitDataReg ( 32 + 46 )

regSPDR :: Reg Uint8
regSPDR = mkReg ( 32 + 46 )

[ivory|
bitdata ACSR :: Bits 8 = acsr
  { acd   :: Bit
  , acbg  :: Bit
  , aco   :: Bit
  , aci   :: Bit
  , acie  :: Bit
  , acic  :: Bit
  , acis1 :: Bit
  , acis0 :: Bit
  }
|]

regBitsACSR :: BitDataReg ACSR
regBitsACSR = mkBitDataReg ( 32 + 48 )

regACSR :: Reg Uint8
regACSR = mkReg ( 32 + 48 )

[ivory|
bitdata SMCR :: Bits 8 = smcr
  { _   :: Bit
  , _   :: Bit
  , _   :: Bit
  , _   :: Bit
  , sm2 :: Bit
  , sm1 :: Bit
  , sm0 :: Bit
  , se  :: Bit
  }
|]

regBitsSMCR :: BitDataReg SMCR
regBitsSMCR = mkBitDataReg ( 32 + 51 )

regSMCR :: Reg Uint8
regSMCR = mkReg ( 32 + 51 )

[ivory|
bitdata MCUSR :: Bits 8 = mcusr
  { _     :: Bit
  , _     :: Bit
  , _     :: Bit
  , _     :: Bit
  , wdrf  :: Bit
  , borf  :: Bit
  , extrf :: Bit
  , porf  :: Bit
  }
|]

regBitsMCUSR :: BitDataReg MCUSR
regBitsMCUSR = mkBitDataReg ( 32 + 52 )

regMCUSR :: Reg Uint8
regMCUSR = mkReg ( 32 + 52 )

[ivory|
bitdata MCUCR :: Bits 8 = mcucr
  { _     :: Bit
  , bods  :: Bit
  , bodse :: Bit
  , pud   :: Bit
  , _     :: Bit
  , _     :: Bit
  , ivsel :: Bit
  , ivce  :: Bit
  }
|]

regBitsMCUCR :: BitDataReg MCUCR
regBitsMCUCR = mkBitDataReg ( 32 + 53 )

regMCUCR :: Reg Uint8
regMCUCR = mkReg ( 32 + 53 )

[ivory|
bitdata SPMCSR :: Bits 8 = spmcsr
  { spmie  :: Bit
  , rwwsb  :: Bit
  , sigrd  :: Bit
  , rwwsre :: Bit
  , blbset :: Bit
  , pgwrt  :: Bit
  , pgers  :: Bit
  , spmen  :: Bit
  }
|]

regBitsSPMCSR :: BitDataReg SPMCSR
regBitsSPMCSR = mkBitDataReg ( 32 + 55 )

regSPMCSR :: Reg Uint8
regSPMCSR = mkReg ( 32 + 55 )

[ivory|
bitdata WDTCSR :: Bits 8 = wdtcsr
  { wdif :: Bit
  , wdie :: Bit
  , wdp3 :: Bit
  , wdce :: Bit
  , wde  :: Bit
  , wdp2 :: Bit
  , wdp1 :: Bit
  , wdp0 :: Bit
  }
|]

regBitsWDTCSR :: BitDataReg WDTCSR
regBitsWDTCSR = mkBitDataReg 96

regWDTCSR :: Reg Uint8
regWDTCSR = mkReg 96

[ivory|
bitdata CLKPR :: Bits 8 = clkpr
  { clkpce :: Bit
  , _      :: Bit
  , _      :: Bit
  , _      :: Bit
  , clkps3 :: Bit
  , clkps2 :: Bit
  , clkps1 :: Bit
  , clkps0 :: Bit
  }
|]

regBitsCLKPR :: BitDataReg CLKPR
regBitsCLKPR = mkBitDataReg 97

regCLKPR :: Reg Uint8
regCLKPR = mkReg 97

[ivory|
bitdata PRR :: Bits 8 = prr
  { prtwi    :: Bit
  , prtim2   :: Bit
  , prtim0   :: Bit
  , _        :: Bit
  , prtim1   :: Bit
  , prspi    :: Bit
  , prusart0 :: Bit
  , pradc    :: Bit
  }
|]

regBitsPRR :: BitDataReg PRR
regBitsPRR = mkBitDataReg 100

regPRR :: Reg Uint8
regPRR = mkReg 100

[ivory|
bitdata OSCCAL :: Bits 8 = osccal
  { cal7 :: Bit
  , cal6 :: Bit
  , cal5 :: Bit
  , cal4 :: Bit
  , cal3 :: Bit
  , cal2 :: Bit
  , cal1 :: Bit
  , cal0 :: Bit
  }
|]

regBitsOSCCAL :: BitDataReg OSCCAL
regBitsOSCCAL = mkBitDataReg 102

regOSCCAL :: Reg Uint8
regOSCCAL = mkReg 102

[ivory|
bitdata PCICR :: Bits 8 = pcicr
  { _     :: Bit
  , _     :: Bit
  , _     :: Bit
  , _     :: Bit
  , _     :: Bit
  , pcie2 :: Bit
  , pcie1 :: Bit
  , pcie0 :: Bit
  }
|]

regBitsPCICR :: BitDataReg PCICR
regBitsPCICR = mkBitDataReg 104

regPCICR :: Reg Uint8
regPCICR = mkReg 104

[ivory|
bitdata EICRA :: Bits 8 = eicra
  { _     :: Bit
  , _     :: Bit
  , _     :: Bit
  , _     :: Bit
  , isc11 :: Bit
  , isc10 :: Bit
  , isc01 :: Bit
  , isc00 :: Bit
  }
|]

regBitsEICRA :: BitDataReg EICRA
regBitsEICRA = mkBitDataReg 105

regEICRA :: Reg Uint8
regEICRA = mkReg 105

[ivory|
bitdata PCMSK0 :: Bits 8 = pcmsk0
  { pcint7 :: Bit
  , pcint6 :: Bit
  , pcint5 :: Bit
  , pcint4 :: Bit
  , pcint3 :: Bit
  , pcint2 :: Bit
  , pcint1 :: Bit
  , pcint0 :: Bit
  }
|]

regBitsPCMSK0 :: BitDataReg PCMSK0
regBitsPCMSK0 = mkBitDataReg 107

regPCMSK0 :: Reg Uint8
regPCMSK0 = mkReg 107

[ivory|
bitdata PCMSK1 :: Bits 8 = pcmsk1
  { _       :: Bit
  , pcint14 :: Bit
  , pcint13 :: Bit
  , pcint12 :: Bit
  , pcint11 :: Bit
  , pcint10 :: Bit
  , pcint9  :: Bit
  , pcint8  :: Bit
  }
|]

regBitsPCMSK1 :: BitDataReg PCMSK1
regBitsPCMSK1 = mkBitDataReg 108

regPCMSK1 :: Reg Uint8
regPCMSK1 = mkReg 108

[ivory|
bitdata PCMSK2 :: Bits 8 = pcmsk2
  { pcint23 :: Bit
  , pcint22 :: Bit
  , pcint21 :: Bit
  , pcint20 :: Bit
  , pcint19 :: Bit
  , pcint18 :: Bit
  , pcint17 :: Bit
  , pcint16 :: Bit
  }
|]

regBitsPCMSK2 :: BitDataReg PCMSK2
regBitsPCMSK2 = mkBitDataReg 109

regPCMSK2 :: Reg Uint8
regPCMSK2 = mkReg 109

[ivory|
bitdata TIMSK0 :: Bits 8 = timsk0
  { _      :: Bit
  , _      :: Bit
  , _      :: Bit
  , _      :: Bit
  , _      :: Bit
  , ocie0b :: Bit
  , ocie0a :: Bit
  , toie0  :: Bit
  }
|]

regBitsTIMSK0 :: BitDataReg TIMSK0
regBitsTIMSK0 = mkBitDataReg 110

regTIMSK0 :: Reg Uint8
regTIMSK0 = mkReg 110

[ivory|
bitdata TIMSK1 :: Bits 8 = timsk1
  { _      :: Bit
  , _      :: Bit
  , icie1  :: Bit
  , _      :: Bit
  , _      :: Bit
  , ocie1b :: Bit
  , ocie1a :: Bit
  , toie1  :: Bit
  }
|]

regBitsTIMSK1 :: BitDataReg TIMSK1
regBitsTIMSK1 = mkBitDataReg 111

regTIMSK1 :: Reg Uint8
regTIMSK1 = mkReg 111

[ivory|
bitdata TIMSK2 :: Bits 8 = timsk2
  { _      :: Bit
  , _      :: Bit
  , _      :: Bit
  , _      :: Bit
  , _      :: Bit
  , ocie2b :: Bit
  , ocie2a :: Bit
  , toie2  :: Bit
  }
|]

regBitsTIMSK2 :: BitDataReg TIMSK2
regBitsTIMSK2 = mkBitDataReg 112

regTIMSK2 :: Reg Uint8
regTIMSK2 = mkReg 112

[ivory|
bitdata ADC :: Bits 16 = adc
  { _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  }
|]

regBitsADC :: BitDataReg ADC
regBitsADC = mkBitDataReg 120

regADC :: Reg Uint16
regADC = mkReg 120

[ivory|
bitdata ADCW :: Bits 16 = adcw
  { _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  }
|]

regBitsADCW :: BitDataReg ADCW
regBitsADCW = mkBitDataReg 120

regADCW :: Reg Uint16
regADCW = mkReg 120

[ivory|
bitdata ADCL :: Bits 8 = adcl
  { adcl7 :: Bit
  , adcl6 :: Bit
  , adcl5 :: Bit
  , adcl4 :: Bit
  , adcl3 :: Bit
  , adcl2 :: Bit
  , adcl1 :: Bit
  , adcl0 :: Bit
  }
|]

regBitsADCL :: BitDataReg ADCL
regBitsADCL = mkBitDataReg 120

regADCL :: Reg Uint8
regADCL = mkReg 120

[ivory|
bitdata ADCH :: Bits 8 = adch
  { adch7 :: Bit
  , adch6 :: Bit
  , adch5 :: Bit
  , adch4 :: Bit
  , adch3 :: Bit
  , adch2 :: Bit
  , adch1 :: Bit
  , adch0 :: Bit
  }
|]

regBitsADCH :: BitDataReg ADCH
regBitsADCH = mkBitDataReg 121

regADCH :: Reg Uint8
regADCH = mkReg 121

[ivory|
bitdata ADCSRA :: Bits 8 = adcsra
  { aden  :: Bit
  , adsc  :: Bit
  , adate :: Bit
  , adif  :: Bit
  , adie  :: Bit
  , adps2 :: Bit
  , adps1 :: Bit
  , adps0 :: Bit
  }
|]

regBitsADCSRA :: BitDataReg ADCSRA
regBitsADCSRA = mkBitDataReg 122

regADCSRA :: Reg Uint8
regADCSRA = mkReg 122

[ivory|
bitdata ADCSRB :: Bits 8 = adcsrb
  { _     :: Bit
  , acme  :: Bit
  , _     :: Bit
  , _     :: Bit
  , _     :: Bit
  , adts2 :: Bit
  , adts1 :: Bit
  , adts0 :: Bit
  }
|]

regBitsADCSRB :: BitDataReg ADCSRB
regBitsADCSRB = mkBitDataReg 123

regADCSRB :: Reg Uint8
regADCSRB = mkReg 123

[ivory|
bitdata ADMUX :: Bits 8 = admux
  { refs1 :: Bit
  , refs0 :: Bit
  , adlar :: Bit
  , _     :: Bit
  , mux3  :: Bit
  , mux2  :: Bit
  , mux1  :: Bit
  , mux0  :: Bit
  }
|]

regBitsADMUX :: BitDataReg ADMUX
regBitsADMUX = mkBitDataReg 124

regADMUX :: Reg Uint8
regADMUX = mkReg 124

[ivory|
bitdata DIDR0 :: Bits 8 = didr0
  { _     :: Bit
  , _     :: Bit
  , adc5d :: Bit
  , adc4d :: Bit
  , adc3d :: Bit
  , adc2d :: Bit
  , adc1d :: Bit
  , adc0d :: Bit
  }
|]

regBitsDIDR0 :: BitDataReg DIDR0
regBitsDIDR0 = mkBitDataReg 126

regDIDR0 :: Reg Uint8
regDIDR0 = mkReg 126

[ivory|
bitdata DIDR1 :: Bits 8 = didr1
  { _     :: Bit
  , _     :: Bit
  , _     :: Bit
  , _     :: Bit
  , _     :: Bit
  , _     :: Bit
  , ain1d :: Bit
  , ain0d :: Bit
  }
|]

regBitsDIDR1 :: BitDataReg DIDR1
regBitsDIDR1 = mkBitDataReg 127

regDIDR1 :: Reg Uint8
regDIDR1 = mkReg 127

[ivory|
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
|]

regBitsTCCR1A :: BitDataReg TCCR1A
regBitsTCCR1A = mkBitDataReg 128

regTCCR1A :: Reg Uint8
regTCCR1A = mkReg 128

[ivory|
bitdata TCCR1B :: Bits 8 = tccr1b
  { icnc1 :: Bit
  , ices1 :: Bit
  , _     :: Bit
  , wgm13 :: Bit
  , wgm12 :: Bit
  , cs12  :: Bit
  , cs11  :: Bit
  , cs10  :: Bit
  }
|]

regBitsTCCR1B :: BitDataReg TCCR1B
regBitsTCCR1B = mkBitDataReg 129

regTCCR1B :: Reg Uint8
regTCCR1B = mkReg 129

[ivory|
bitdata TCCR1C :: Bits 8 = tccr1c
  { foc1a :: Bit
  , foc1b :: Bit
  , _     :: Bit
  , _     :: Bit
  , _     :: Bit
  , _     :: Bit
  , _     :: Bit
  , _     :: Bit
  }
|]

regBitsTCCR1C :: BitDataReg TCCR1C
regBitsTCCR1C = mkBitDataReg 130

regTCCR1C :: Reg Uint8
regTCCR1C = mkReg 130

[ivory|
bitdata TCNT1 :: Bits 16 = tcnt1
  { _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  }
|]

regBitsTCNT1 :: BitDataReg TCNT1
regBitsTCNT1 = mkBitDataReg 132

regTCNT1 :: Reg Uint16
regTCNT1 = mkReg 132

[ivory|
bitdata TCNT1L :: Bits 8 = tcnt1l
  { tcnt1l7 :: Bit
  , tcnt1l6 :: Bit
  , tcnt1l5 :: Bit
  , tcnt1l4 :: Bit
  , tcnt1l3 :: Bit
  , tcnt1l2 :: Bit
  , tcnt1l1 :: Bit
  , tcnt1l0 :: Bit
  }
|]

regBitsTCNT1L :: BitDataReg TCNT1L
regBitsTCNT1L = mkBitDataReg 132

regTCNT1L :: Reg Uint8
regTCNT1L = mkReg 132

[ivory|
bitdata TCNT1H :: Bits 8 = tcnt1h
  { tcnt1h7 :: Bit
  , tcnt1h6 :: Bit
  , tcnt1h5 :: Bit
  , tcnt1h4 :: Bit
  , tcnt1h3 :: Bit
  , tcnt1h2 :: Bit
  , tcnt1h1 :: Bit
  , tcnt1h0 :: Bit
  }
|]

regBitsTCNT1H :: BitDataReg TCNT1H
regBitsTCNT1H = mkBitDataReg 133

regTCNT1H :: Reg Uint8
regTCNT1H = mkReg 133

[ivory|
bitdata ICR1 :: Bits 16 = icr1
  { _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  }
|]

regBitsICR1 :: BitDataReg ICR1
regBitsICR1 = mkBitDataReg 134

regICR1 :: Reg Uint16
regICR1 = mkReg 134

[ivory|
bitdata ICR1L :: Bits 8 = icr1l
  { icr1l7 :: Bit
  , icr1l6 :: Bit
  , icr1l5 :: Bit
  , icr1l4 :: Bit
  , icr1l3 :: Bit
  , icr1l2 :: Bit
  , icr1l1 :: Bit
  , icr1l0 :: Bit
  }
|]

regBitsICR1L :: BitDataReg ICR1L
regBitsICR1L = mkBitDataReg 134

regICR1L :: Reg Uint8
regICR1L = mkReg 134

[ivory|
bitdata ICR1H :: Bits 8 = icr1h
  { icr1h7 :: Bit
  , icr1h6 :: Bit
  , icr1h5 :: Bit
  , icr1h4 :: Bit
  , icr1h3 :: Bit
  , icr1h2 :: Bit
  , icr1h1 :: Bit
  , icr1h0 :: Bit
  }
|]

regBitsICR1H :: BitDataReg ICR1H
regBitsICR1H = mkBitDataReg 135

regICR1H :: Reg Uint8
regICR1H = mkReg 135

[ivory|
bitdata OCR1A :: Bits 16 = ocr1a
  { _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  }
|]

regBitsOCR1A :: BitDataReg OCR1A
regBitsOCR1A = mkBitDataReg 136

regOCR1A :: Reg Uint16
regOCR1A = mkReg 136

[ivory|
bitdata OCR1AL :: Bits 8 = ocr1al
  { ocr1al7 :: Bit
  , ocr1al6 :: Bit
  , ocr1al5 :: Bit
  , ocr1al4 :: Bit
  , ocr1al3 :: Bit
  , ocr1al2 :: Bit
  , ocr1al1 :: Bit
  , ocr1al0 :: Bit
  }
|]

regBitsOCR1AL :: BitDataReg OCR1AL
regBitsOCR1AL = mkBitDataReg 136

regOCR1AL :: Reg Uint8
regOCR1AL = mkReg 136

[ivory|
bitdata OCR1AH :: Bits 8 = ocr1ah
  { ocr1ah7 :: Bit
  , ocr1ah6 :: Bit
  , ocr1ah5 :: Bit
  , ocr1ah4 :: Bit
  , ocr1ah3 :: Bit
  , ocr1ah2 :: Bit
  , ocr1ah1 :: Bit
  , ocr1ah0 :: Bit
  }
|]

regBitsOCR1AH :: BitDataReg OCR1AH
regBitsOCR1AH = mkBitDataReg 137

regOCR1AH :: Reg Uint8
regOCR1AH = mkReg 137

[ivory|
bitdata OCR1B :: Bits 16 = ocr1b
  { _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  }
|]

regBitsOCR1B :: BitDataReg OCR1B
regBitsOCR1B = mkBitDataReg 138

regOCR1B :: Reg Uint16
regOCR1B = mkReg 138

[ivory|
bitdata OCR1BL :: Bits 8 = ocr1bl
  { ocr1bl7 :: Bit
  , ocr1bl6 :: Bit
  , ocr1bl5 :: Bit
  , ocr1bl4 :: Bit
  , ocr1bl3 :: Bit
  , ocr1bl2 :: Bit
  , ocr1bl1 :: Bit
  , ocr1bl0 :: Bit
  }
|]

regBitsOCR1BL :: BitDataReg OCR1BL
regBitsOCR1BL = mkBitDataReg 138

regOCR1BL :: Reg Uint8
regOCR1BL = mkReg 138

[ivory|
bitdata OCR1BH :: Bits 8 = ocr1bh
  { ocr1bh7 :: Bit
  , ocr1bh6 :: Bit
  , ocr1bh5 :: Bit
  , ocr1bh4 :: Bit
  , ocr1bh3 :: Bit
  , ocr1bh2 :: Bit
  , ocr1bh1 :: Bit
  , ocr1bh0 :: Bit
  }
|]

regBitsOCR1BH :: BitDataReg OCR1BH
regBitsOCR1BH = mkBitDataReg 139

regOCR1BH :: Reg Uint8
regOCR1BH = mkReg 139

[ivory|
bitdata TCCR2A :: Bits 8 = tccr2a
  { com2a1 :: Bit
  , com2a0 :: Bit
  , com2b1 :: Bit
  , com2b0 :: Bit
  , _      :: Bit
  , _      :: Bit
  , wgm21  :: Bit
  , wgm20  :: Bit
  }
|]

regBitsTCCR2A :: BitDataReg TCCR2A
regBitsTCCR2A = mkBitDataReg 176

regTCCR2A :: Reg Uint8
regTCCR2A = mkReg 176

[ivory|
bitdata TCCR2B :: Bits 8 = tccr2b
  { foc2a :: Bit
  , foc2b :: Bit
  , _     :: Bit
  , _     :: Bit
  , wgm22 :: Bit
  , cs22  :: Bit
  , cs21  :: Bit
  , cs20  :: Bit
  }
|]

regBitsTCCR2B :: BitDataReg TCCR2B
regBitsTCCR2B = mkBitDataReg 177

regTCCR2B :: Reg Uint8
regTCCR2B = mkReg 177

[ivory|
bitdata TCNT2 :: Bits 8 = tcnt2
  { tcnt2_7 :: Bit
  , tcnt2_6 :: Bit
  , tcnt2_5 :: Bit
  , tcnt2_4 :: Bit
  , tcnt2_3 :: Bit
  , tcnt2_2 :: Bit
  , tcnt2_1 :: Bit
  , tcnt2_0 :: Bit
  }
|]

regBitsTCNT2 :: BitDataReg TCNT2
regBitsTCNT2 = mkBitDataReg 178

regTCNT2 :: Reg Uint8
regTCNT2 = mkReg 178

[ivory|
bitdata OCR2A :: Bits 8 = ocr2a
  { ocr2_7_a :: Bit
  , ocr2_6_a :: Bit
  , ocr2_5_a :: Bit
  , ocr2_4_a :: Bit
  , ocr2_3_a :: Bit
  , ocr2_2_a :: Bit
  , ocr2_1_a :: Bit
  , ocr2_0_a :: Bit
  }
|]

regBitsOCR2A :: BitDataReg OCR2A
regBitsOCR2A = mkBitDataReg 179

regOCR2A :: Reg Uint8
regOCR2A = mkReg 179

[ivory|
bitdata OCR2B :: Bits 8 = ocr2b
  { ocr2_7_b :: Bit
  , ocr2_6_b :: Bit
  , ocr2_5_b :: Bit
  , ocr2_4_b :: Bit
  , ocr2_3_b :: Bit
  , ocr2_2_b :: Bit
  , ocr2_1_b :: Bit
  , ocr2_0_b :: Bit
  }
|]

regBitsOCR2B :: BitDataReg OCR2B
regBitsOCR2B = mkBitDataReg 180

regOCR2B :: Reg Uint8
regOCR2B = mkReg 180

[ivory|
bitdata ASSR :: Bits 8 = assr
  { _       :: Bit
  , exclk   :: Bit
  , as2     :: Bit
  , tcn2ub  :: Bit
  , ocr2aub :: Bit
  , ocr2bub :: Bit
  , tcr2aub :: Bit
  , tcr2bub :: Bit
  }
|]

regBitsASSR :: BitDataReg ASSR
regBitsASSR = mkBitDataReg 182

regASSR :: Reg Uint8
regASSR = mkReg 182

[ivory|
bitdata TWBR :: Bits 8 = twbr
  { twbr7 :: Bit
  , twbr6 :: Bit
  , twbr5 :: Bit
  , twbr4 :: Bit
  , twbr3 :: Bit
  , twbr2 :: Bit
  , twbr1 :: Bit
  , twbr0 :: Bit
  }
|]

regBitsTWBR :: BitDataReg TWBR
regBitsTWBR = mkBitDataReg 184

regTWBR :: Reg Uint8
regTWBR = mkReg 184

[ivory|
bitdata TWSR :: Bits 8 = twsr
  { tws7  :: Bit
  , tws6  :: Bit
  , tws5  :: Bit
  , tws4  :: Bit
  , tws3  :: Bit
  , _     :: Bit
  , twps1 :: Bit
  , twps0 :: Bit
  }
|]

regBitsTWSR :: BitDataReg TWSR
regBitsTWSR = mkBitDataReg 185

regTWSR :: Reg Uint8
regTWSR = mkReg 185

[ivory|
bitdata TWAR :: Bits 8 = twar
  { twa6  :: Bit
  , twa5  :: Bit
  , twa4  :: Bit
  , twa3  :: Bit
  , twa2  :: Bit
  , twa1  :: Bit
  , twa0  :: Bit
  , twgce :: Bit
  }
|]

regBitsTWAR :: BitDataReg TWAR
regBitsTWAR = mkBitDataReg 186

regTWAR :: Reg Uint8
regTWAR = mkReg 186

[ivory|
bitdata TWDR :: Bits 8 = twdr
  { twd7 :: Bit
  , twd6 :: Bit
  , twd5 :: Bit
  , twd4 :: Bit
  , twd3 :: Bit
  , twd2 :: Bit
  , twd1 :: Bit
  , twd0 :: Bit
  }
|]

regBitsTWDR :: BitDataReg TWDR
regBitsTWDR = mkBitDataReg 187

regTWDR :: Reg Uint8
regTWDR = mkReg 187

[ivory|
bitdata TWCR :: Bits 8 = twcr
  { twint :: Bit
  , twea  :: Bit
  , twsta :: Bit
  , twsto :: Bit
  , twwc  :: Bit
  , twen  :: Bit
  , _     :: Bit
  , twie  :: Bit
  }
|]

regBitsTWCR :: BitDataReg TWCR
regBitsTWCR = mkBitDataReg 188

regTWCR :: Reg Uint8
regTWCR = mkReg 188

[ivory|
bitdata TWAMR :: Bits 8 = twamr
  { _     :: Bit
  , twam6 :: Bit
  , twam5 :: Bit
  , twam4 :: Bit
  , twam3 :: Bit
  , twam2 :: Bit
  , twam1 :: Bit
  , twam0 :: Bit
  }
|]

regBitsTWAMR :: BitDataReg TWAMR
regBitsTWAMR = mkBitDataReg 189

regTWAMR :: Reg Uint8
regTWAMR = mkReg 189

[ivory|
bitdata UCSR0A :: Bits 8 = ucsr0a
  { rxc0  :: Bit
  , txc0  :: Bit
  , udre0 :: Bit
  , fe0   :: Bit
  , dor0  :: Bit
  , upe0  :: Bit
  , u2x0  :: Bit
  , mpcm0 :: Bit
  }
|]

regBitsUCSR0A :: BitDataReg UCSR0A
regBitsUCSR0A = mkBitDataReg 192

regUCSR0A :: Reg Uint8
regUCSR0A = mkReg 192

[ivory|
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
|]

regBitsUCSR0B :: BitDataReg UCSR0B
regBitsUCSR0B = mkBitDataReg 193

regUCSR0B :: Reg Uint8
regUCSR0B = mkReg 193

[ivory|
bitdata UCSR0C :: Bits 8 = ucsr0c
  { umsel01 :: Bit
  , umsel00 :: Bit
  , upm01   :: Bit
  , upm00   :: Bit
  , usbs0   :: Bit
  , udord0  :: Bit
  , ucpha0  :: Bit
  , ucpol0  :: Bit
  }
|]

regBitsUCSR0C :: BitDataReg UCSR0C
regBitsUCSR0C = mkBitDataReg 194

regUCSR0C :: Reg Uint8
regUCSR0C = mkReg 194

[ivory|
bitdata UBRR0 :: Bits 16 = ubrr0
  { _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  , _ :: Bit
  }
|]

regBitsUBRR0 :: BitDataReg UBRR0
regBitsUBRR0 = mkBitDataReg 196

regUBRR0 :: Reg Uint16
regUBRR0 = mkReg 196

[ivory|
bitdata UBRR0L :: Bits 8 = ubrr0l
  { ubrr0_7 :: Bit
  , ubrr0_6 :: Bit
  , ubrr0_5 :: Bit
  , ubrr0_4 :: Bit
  , ubrr0_3 :: Bit
  , ubrr0_2 :: Bit
  , ubrr0_1 :: Bit
  , ubrr0_0 :: Bit
  }
|]

regBitsUBRR0L :: BitDataReg UBRR0L
regBitsUBRR0L = mkBitDataReg 196

regUBRR0L :: Reg Uint8
regUBRR0L = mkReg 196

[ivory|
bitdata UBRR0H :: Bits 8 = ubrr0h
  { _        :: Bit
  , _        :: Bit
  , _        :: Bit
  , _        :: Bit
  , ubrr0_11 :: Bit
  , ubrr0_10 :: Bit
  , ubrr0_9  :: Bit
  , ubrr0_8  :: Bit
  }
|]

regBitsUBRR0H :: BitDataReg UBRR0H
regBitsUBRR0H = mkBitDataReg 197

regUBRR0H :: Reg Uint8
regUBRR0H = mkReg 197

[ivory|
bitdata UDR0 :: Bits 8 = udr0
  { udr0_7 :: Bit
  , udr0_6 :: Bit
  , udr0_5 :: Bit
  , udr0_4 :: Bit
  , udr0_3 :: Bit
  , udr0_2 :: Bit
  , udr0_1 :: Bit
  , udr0_0 :: Bit
  }
|]

regBitsUDR0 :: BitDataReg UDR0
regBitsUDR0 = mkBitDataReg 198

regUDR0 :: Reg Uint8
regUDR0 = mkReg 198

