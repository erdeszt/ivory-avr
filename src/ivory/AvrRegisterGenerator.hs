{-# LANGUAGE OverloadedStrings #-}

module AvrRegisterGenerator where

import Control.Applicative

import Data.List (foldl')

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Vector (Vector)
import qualified Data.Vector as Vector

import Data.Char (isSpace)

import Data.Attoparsec.Text

newtype Label = Label Text
    deriving (Show, Eq)

newtype Location = Location Int
    deriving (Show, Eq)

newtype BitLocation = BitLocation Int
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
    let lines = tail $ filter (Text.isPrefixOf "#define") (Text.lines rawInput)
    let parseResult = foldl' propagateParseErrors (Right (ParserState Skip Vector.empty)) lines
    registers <$> parseResult
  where
    propagateParseErrors state line = state >>= (`parseLines` line)
    parseLines :: ParserState -> Text -> Either ParserError ParserState
    parseLines (ParserState phase regs) line = do
        let parseResult = parseOnly parseDefinition line
        case (phase, parseResult, Text.isInfixOf "vect" line) of
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

demo :: IO ()
demo = do
    raw <- TextIO.readFile "iom328p.h"
    let parseResult = avrRegisterParser raw
    case parseResult of
        Left error -> print error
        Right regs ->
            putStrLn
            $ unlines
            $ map ppReg (Vector.toList regs)
    return ()
  where
    ppReg (Register (RegisterDecl regLabel regType regLocation) bits) =
        unwords
            [ show regLabel
            , show regType
            , show regLocation
            , unwords (show <$> Map.toList bits)
            ]
