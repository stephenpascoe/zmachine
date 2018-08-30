{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( parseHeader
    , open
    , Header(..)
    -- , parseZchars
    , showHeader
    ) where

import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get
import Data.Word
import Data.Int
import Data.Bits
import Data.List
import Data.Monoid
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Char as C
import qualified Formatting as F
import Formatting ((%), (%.))


type ByteAddress = Word16
type Colour = Word8

-- TODO : Use real data types for flags
type Flags1 = Word8
type Flags2 = Word8

data Header = Header { version :: Int8
                     , flags1 :: Flags1   -- Mutable
                     , baseHighMemory :: ByteAddress
                     , initPC :: Word16   -- Packed address in V6
                     , dictionary :: ByteAddress
                     , objectTable :: ByteAddress
                     , variablesTable :: ByteAddress
                     , baseStaticMemory :: ByteAddress
                     , flags2 :: Flags2   -- Mutable
                     , abbreviationsTable :: ByteAddress
                     , fileLength :: Word32
                     , checksum :: Word16
                     , interpreterNumber :: Int8
                     , interpreterVersion :: Int8
                     , screenWidth :: Int16 -- units or chars depending on version
                     , screenHeight :: Int16
                     , fontWidth :: Int16
                     , fontHight :: Int16
                     , routinesOffset :: Word16
                     , staticStringsOffset :: Word16
                     , backgroundColour :: Colour -- Mutable
                     , foregroundColour :: Colour -- Mutable
                     , endCharacterTable :: ByteAddress
                     , stream3OutputPixels :: Int16 -- Mutable
                     , revisionNumber :: Int8 -- Mutable
                     , alphabetTable :: ByteAddress
                     , extensionTable :: ByteAddress
                     } deriving Show

defaultHeader = Header { version = 0
                       , flags1 = 0
                       , baseHighMemory = 0
                       , initPC = 0
                       , dictionary = 0
                       , objectTable = 0
                       , variablesTable = 0
                       , baseStaticMemory = 0
                       , flags2 = 0
                       , abbreviationsTable = 0
                       , fileLength = 0
                       , checksum = 0
                       , interpreterNumber = 0
                       , interpreterVersion = 0
                       , screenWidth = 0
                       , screenHeight = 0
                       , fontWidth = 0
                       , fontHight = 0
                       , routinesOffset = 0
                       , staticStringsOffset = 0
                       , backgroundColour = 0
                       , foregroundColour = 0
                       , endCharacterTable = 0
                       , stream3OutputPixels = 0
                       , revisionNumber = 0
                       , alphabetTable = 0
                       , extensionTable = 0
                       }


parseHeader :: Get Header
parseHeader = do
  version <- getInt8
  flags1 <- getWord8
  -- Conventional use so discarded
  skip 2
  baseHighMemory <- getWord16be
  initPC <- getWord16be
  dictionary <- getWord16be
  objectTable <- getWord16be
  variablesTable <- getWord16be
  baseStaticMemory <- getWord16be
  flags2 <- getWord8
  skip 7
  abbreviationsTable <- getWord16be
  rawFileLength <- getWord16be
  checksum <- getWord16be
  interpreterNumber <- getInt8
  interpreterVersion <- getInt8
  -- TODO Screen and font fields
  skip 8
  routinesOffset <- getWord16be
  staticStringsOffset <- getWord16be
  -- TODO Rest of header

  let fileLength = scale (fromIntegral rawFileLength) where
        scale :: Word32 -> Word32
        scale x | version < 4 = x * 2
        scale x | version < 6 = x * 4
        scale x = x * 8

  return defaultHeader { version
                       , flags1
                       , baseHighMemory
                       , initPC
                       , dictionary
                       , objectTable
                       , variablesTable
                       , baseStaticMemory
                       , flags2
                       , abbreviationsTable
                       , fileLength
                       , checksum
                       , interpreterNumber
                       , interpreterVersion
                       }


open :: FilePath -> IO Header
open path = do
  storyFile <- BL.readFile path
  return $ runGet parseHeader storyFile


showHeader :: Header -> TL.Text
showHeader header = TB.toLazyText $ mconcat [ bprint "Z-code version" version F.int
                                            , bprint "Interpreter flags" flags1 F.int
                                            -- TODO : release number
                                            , bprint "Size of resident memory" baseHighMemory F.hex
                                            , bprint "Start PC" initPC F.hex
                                            , bprint "Dictionary address" dictionary F.hex
                                            , bprint "Object table address" objectTable F.hex
                                            , bprint "Global variables address" variablesTable F.hex
                                            , bprint "Size of dynamic memory" baseStaticMemory F.hex
                                            , bprint "Game flags" flags2 F.int
                                            -- TODO : serial number
                                            , bprint "Abbreviations address" abbreviationsTable F.hex
                                            , bprint "File size" fileLength F.hex
                                            -- TODO : Checksum
                                            , bprint "Checksum" checksum F.hex
                                            -- TODO : Terminating keys
                                            -- TODO : Header extension
                                            , bprint "Inform Version" interpreterNumber F.int
                                            ]
  where
    field typ = (F.right 26 ' ' %. F.text % ": ") % typ % "\n"
    bprint fieldName accessor fieldType   = F.bprint (field fieldType) fieldName (accessor header)


{-
--
-- ZSCII handling
--

type Zchar = Word8
type ZSCII = C.Char

data CurrentAlphabet = A0 | A1 | A2 deriving Eq

data Alphabet = Alphabet { alpha0 :: T.Text
                         , alpha1 :: T.Text
                         , alpha2 :: T.Text
                         }

alphabetV1 :: Alphabet
alphabetV1 = Alphabet { alpha0 = "abcdefghijklmnopqrstuvwxyz"
                      , alpha1 = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                      , alpha2 = " 0123456789.,!?_#'\"/\\<-:()"
                      }

alphabetV2 :: Alphabet
alphabetV2 = Alphabet { alpha0 = "abcdefghijklmnopqrstuvwxyz"
                      , alpha1 = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                      , alpha2 = " ^0123456789.,!?_#'\"/\\-:()"
                      }

-- TODO : Version 5 supports an alphabet table in the file.  This is assumed to be absent.

data DecodeState = DecodeState { alphabet :: Alphabet
                               , currentAlphabet :: CurrentAlphabet
                               , lockAlphabet :: CurrentAlphabet
                               , decoded :: [ZSCII]
                               }

decodeZchar :: DecodeState -> Zchar -> DecodeState
decodeZchar state c = if c < 6 then shift else lookup where
  shift = case c of
            -- TODO : c == 1 or 0
            2 -> state { shiftMode = Shift2, alphabet = cycle2 (alphabet state) }
            3 -> state { shiftMode = Shift3, alphabet = cycle3 (alphabet state) }
            -- TODO
  unshift = undefined
  lookup = unshift $ case currentAlphabet state of
                       A0 -> state { decoded = (getChar alpha0:decoded state)
                                   }
                       A1 -> state { decoded = (getChar alpha1:decoded state)
                                   }
                       A2 -> state { decoded = (getChar alpha2:decoded state)
                                   }
  getChar f = T.index (f (alphabet state)) (fromIntegral c)
  cycle2 alpha = head . tail . dropWhile (/= alpha) $ cycle [A0, A1, A2]
  cycle3 alpha = head . tail . tail . dropWhile (/= alpha) $ cycle [A0, A1, A2]
  uncycle2 alpha = head . tail . dropWhile (/= alpha) $ cycle [A2, A1, A0]
  uncycle3 alpha = head . tail . tail . dropWhile (/= alpha) $ cycle [A2, A1, A0]

parseZchars :: Get [Zchar]
parseZchars = do
  empty <- isEmpty
  if empty
    then return []
    else do w <- getWord16be
            let (z1, z2, z3) = unpackZchars w
            -- End of Z-string
            if (w .&. 0x8000) == 0x8000
              then return [z1,z2,z3]
              else do zchars <- parseZchars
                      return (z1:z2:z3:zchars)


unpackZchars :: Word16 -> (Zchar, Zchar, Zchar)
unpackZchars w = (z1, z2, z3) where
  mask = 0x001f
  w' = w `xor` 0x8000
  z1 = fromIntegral $ (w' `shift` (-10)) .&. mask
  z2 = fromIntegral $ (w' `shift`  (-5)) .&. mask
  z3 = fromIntegral $ w' .&. mask
-}
