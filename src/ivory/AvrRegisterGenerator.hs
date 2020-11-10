{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

data ParserState
  = Skip
  | Reg RegisterDecl
  | RegBit RegisterDecl (Map BitLocation Label)
  deriving (Show, Eq)

parseRegisterDecl :: Parser RegisterDecl
parseRegisterDecl = do
    string "#define"
    skipSpace
    label <- Label <$> takeTill isSpace
    skipSpace
    regType <- choice
        [ (string "_SFR_IO8" *> pure IO8)
        , (string "_SFR_IO16" *> pure IO16)
        , (string "_SFR_MEM8" *> pure MEM8)
        , (string "_SFR_MEM16" *> pure MEM16)
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

-- TODO:
-- Fill out empty bits
-- separate by SFR_IO vs SFR_MEM 8 vs 16 bit
avrRegisterParser :: FilePath -> IO (Vector Register)
avrRegisterParser inputFile = do
    lines <- filter (Text.isPrefixOf "#define") <$> Text.lines <$> TextIO.readFile inputFile
    -- foldl'
    return Vector.empty

demo :: IO ()
demo = do
    defs <- avrRegisterParser "iom328p.h"
    putStrLn (unlines (map show (Vector.toList defs)))
    return ()
