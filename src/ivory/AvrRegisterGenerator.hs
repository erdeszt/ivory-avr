{-# LANGUAGE OverloadedStrings #-}

module AvrRegisterGenerator where

import Control.Applicative

import Data.List (foldl')

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Vector (Vector)
import qualified Data.Vector as Vector

import Data.Char (isSpace)

import Data.Attoparsec.Text

newtype Label = Label { unLabel :: Text }
    deriving (Show, Eq)

newtype Location = Location Int
    deriving (Show, Eq)

newtype BitLocation = BitLocation Int
    deriving (Show, Eq, Ord)

newtype IOOffset = IOOffset Int
    deriving (Show, Eq, Ord)

data RegType = IO8 | IO16 | MEM8 | MEM16
    deriving (Show, Eq)

data RegisterDecl = RegisterDecl Label RegType Location
    deriving (Show, Eq)

data Register = Register RegisterDecl (Map BitLocation Label)
    deriving (Show, Eq)

data ParserPhase
  = Skip
  | End
  | RegBit RegisterDecl (Map BitLocation Label)
  deriving (Show, Eq)

data ParserState = ParserState { phase :: ParserPhase, registers :: Vector Register }
    deriving (Show)

data ParserError = RegBitWithoutRegDecl (Vector Register) (Label, BitLocation)
    deriving (Show)

parseRegisterDecl :: Parser RegisterDecl
parseRegisterDecl = do
    string "#define"
    skipSpace
    label <- Label <$> takeTill isSpace
    skipSpace
    regType <- choice
        [ string "_SFR_IO8" *> pure IO8
        , string "_SFR_IO16" *> pure IO16
        , string "_SFR_MEM8" *> pure MEM8
        , string "_SFR_MEM16" *> pure MEM16
        ]
    char '('
    string "0x"
    location <- Location <$> hexadecimal
    char ')'
    return (RegisterDecl label regType location)

parseRegisterBitLocation :: Parser (Label, BitLocation)
parseRegisterBitLocation = do
    string "#define"
    skipSpace
    label <- Label <$> takeTill isSpace
    skipSpace
    location <- BitLocation <$> decimal
    return (label, location)

parseDefinition :: Parser (Either RegisterDecl (Label, BitLocation))
parseDefinition = (Left <$> parseRegisterDecl) <|> (Right <$> parseRegisterBitLocation)

-- NOTE: Everything after the first interrupt vector definition is ignored
avrRegisterParser :: Text -> Either ParserError (Vector Register)
avrRegisterParser rawInput = do
    -- Tail drops the header guard
    let lines = tail $ filter definitionFilter (Text.lines rawInput)
    let parseResult = foldl' propagateParseErrors (Right (ParserState Skip Vector.empty)) lines
    registers <$> parseResult
  where
    definitionFilter line =
        Text.isPrefixOf "#define" line && not (Text.isInfixOf "_EEPROM_REG_LOCATIONS_" line)
    propagateParseErrors state line = state >>= (`parseLines` line)
    parseLines :: ParserState -> Text -> Either ParserError ParserState
    parseLines (ParserState phase regs) line = do
        let parseResult = parseOnly parseDefinition line
        case (phase, parseResult, Text.isInfixOf "vect" line) of
            (RegBit reg bits, _, True) ->
                Right (ParserState End (Vector.snoc regs (Register reg bits)))
            (_, _, True) ->
                Right (ParserState End regs)
            (End, _, _) ->
                Right (ParserState End regs)
            (Skip, Left _, _) ->
                Right (ParserState Skip regs)
            (Skip, Right (Left regDecl), _) ->
                Right (ParserState (RegBit regDecl Map.empty) regs)
            (Skip, Right (Right regBit), _) ->
                Left (RegBitWithoutRegDecl regs regBit)
            (RegBit reg bits, Left _, _) -> do
                let regs' = Vector.snoc regs (Register reg bits)
                Right (ParserState Skip regs')
            (RegBit reg bits, Right (Left regDecl), _) -> do
                let regs' = Vector.snoc regs (Register reg bits)
                let phase' = RegBit regDecl Map.empty
                Right (ParserState phase' regs')
            (RegBit reg bits, Right (Right (label, location)), _) -> do
                let bits' = Map.insert location label bits
                let phase' = RegBit reg bits'
                Right (ParserState phase' regs)

-- TODO: Improve duplicate resolution for OCR2(A|B) bits to match the C defs
generateIvoryForReg :: IOOffset -> Register -> Text
generateIvoryForReg (IOOffset iooffset) (Register (RegisterDecl (Label regLabel) regType (Location regLocation)) bits) = do
    Text.unlines $
        [ "[ivory|"
        , Text.unwords
            ["bitdata"
            , regLabel
            , "::"
            , "Bits"
            , Text.pack (show bitCount)
            , "="
            , Text.toLower regLabel
            ]
        ]
        ++ map generateBit (reverse [0..lastBit])
        ++ [ Text.append indent "}"
           , "|]"
           , ""
           , Text.unwords [bitDataReg, "::", "BitDataReg", regLabel]
           , Text.unwords [bitDataReg, "=", "mkBitDataReg", adjustedRegLocation]
           , ""
           , Text.unwords [dataReg, "::", "Reg", regIntType]
           , Text.unwords [dataReg, "=", "mkReg", adjustedRegLocation]
           ]
  where
    showT :: (Show a) => a -> Text
    showT = Text.pack . show
    regIntType :: Text
    regIntType =
        case regType of
            IO8   -> "Uint8"
            IO16  -> "Uint16"
            MEM8  -> "Uint8"
            MEM16 -> "Uint16"
    adjustedRegLocation :: Text
    adjustedRegLocation =
        case regType of
            IO8   -> Text.unwords ["(", showT iooffset, "+", showT regLocation, ")"]
            IO16  -> Text.unwords ["(", showT iooffset, "+", showT regLocation, ")"]
            MEM8  -> showT regLocation
            MEM16 -> showT regLocation
    longestLabel :: Int
    longestLabel = do
        let labelLengths = (map (Text.length . unLabel) (Map.elems bits))
        case labelLengths of
            [] -> 1
            _ -> maximum labelLengths
    padTo :: Int -> Text -> Text
    padTo desiredLength text = do
        let textLength = Text.length text
        if textLength >= desiredLength
            then text
            else text `Text.append` Text.replicate (desiredLength - textLength) " "
    bitDataReg :: Text
    bitDataReg = Text.append "regBits" regLabel
    dataReg :: Text
    dataReg = Text.append  "reg" regLabel
    indent :: Text
    indent = "  "
    bitCount :: Int
    bitCount =
        case regType of
            IO8   -> 8
            IO16  -> 16
            MEM8  -> 8
            MEM16 -> 16
    lastBit :: Int
    lastBit = bitCount - 1
    resolveDuplicate :: Text -> Text -> Text
    resolveDuplicate regLabel bitLabel =
        case regLabel of
            "OCR2A" -> Text.snoc  bitLabel 'A'
            "OCR2B" -> Text.snoc  bitLabel 'B'
            _ -> bitLabel
    generateBit :: Int -> Text
    generateBit bitIndex = do
        let sep = if bitIndex == lastBit then "{" else ","
        let bitLabel = case Map.lookup (BitLocation bitIndex) bits of
                    Nothing ->  padTo longestLabel "_"
                    Just (Label rawLabel) -> padTo longestLabel (Text.toLower (resolveDuplicate regLabel rawLabel))
        Text.append indent (Text.unwords [sep, bitLabel, ":: Bit"])


demo :: IO ()
demo = do
    raw <- TextIO.readFile "iom328p.h"
    let atmega328pIOOffset = IOOffset 0x20
    let parseResult = avrRegisterParser raw
    case parseResult of
        Left error -> print error
        Right regs -> do
            TextIO.writeFile "atmega328pRegs.hs"
            $ Text.unlines
            $ map (generateIvoryForReg atmega328pIOOffset) (Vector.toList regs)
    return ()