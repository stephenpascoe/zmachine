{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib.Memory
  ( Handle
  , Header
  , new
  , close
  , getHeader
  , getStoryBytes
  , showHeader
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Encoding as TE
import qualified Data.Char as C
import qualified Formatting as F
import Formatting ((%), (%.))
import Data.Binary.Get
import Data.Word
import Data.Int
import Data.Bits
import Data.List
import Data.Monoid


-- TODO : Newtupe deriving
-- newtype ZMem a = ZMem (ReaderT B.ByteString IO a) deriving (Functor, Monad, Applicative, MonadReader B.ByteString)
newtype Handle = Handle { storyBytes :: B.ByteString }


type ByteAddress = Word16
type Colour = Word8

-- TODO : Use real data types for flags
type Flags1 = Word8
type Flags2 = Word8

data Header = Header { version :: Int8
                     , flags1 :: Flags1   -- Mutable
                     , releaseNumber :: Word16
                     , baseHighMemory :: ByteAddress
                     , initPC :: Word16   -- Packed address in V6
                     , dictionary :: ByteAddress
                     , objectTable :: ByteAddress
                     , variablesTable :: ByteAddress
                     , baseStaticMemory :: ByteAddress
                     , flags2 :: Flags2   -- Mutable
                     , serialCode :: B.ByteString
                     , abbreviationsTable :: ByteAddress
                     , fileLength :: Word32
                     , checksum :: Word16
                     , interpreterNumber :: Word8
                     , interpreterVersion :: Word8
                     , screenHeightLines :: Word8
                     , screenWidthChars :: Word8
                     , screenWidth :: Word16 -- units or chars depending on version
                     , screenHeight :: Word16
                     , fontWidth :: Word8
                     , fontHeight :: Word8
                     , routinesOffset :: Word16
                     , staticStringsOffset :: Word16
                     , backgroundColour :: Colour -- Mutable
                     , foregroundColour :: Colour -- Mutable
                     , endCharacterTable :: ByteAddress
                     , stream3OutputPixels :: Word16 -- Mutable
                     , revisionNumber :: Word16 -- Mutable
                     , alphabetTable :: ByteAddress
                     , extensionTable :: ByteAddress
                     } deriving Show



new :: FilePath -> IO Handle
new path = do file <- BL.readFile path
              return $ Handle (BL.toStrict file)

close :: Handle -> IO ()
close _ = return ()

getStoryBytes :: Handle
         -> Int -- ^ Offset
         -> Int -- ^ Length
         -> B.ByteString
getStoryBytes h offset length = B.take length $ B.drop offset (storyBytes h)

getHeader :: Handle -> Header
getHeader h = runGet parseHeader (BL.fromStrict $ storyBytes h)

parseHeader :: Get Header
parseHeader = do
  version <- getInt8
  flags1 <- getWord8
  releaseNumber <- getWord16be
  baseHighMemory <- getWord16be
  initPC <- getWord16be
  dictionary <- getWord16be
  objectTable <- getWord16be
  variablesTable <- getWord16be
  baseStaticMemory <- getWord16be
  flags2 <- getWord8
  skip 1
  serialCode <- getByteString 6
  abbreviationsTable <- getWord16be
  rawFileLength <- getWord16be
  checksum <- getWord16be
  interpreterNumber <- getWord8
  interpreterVersion <- getWord8
  screenHeightLines <- getWord8
  screenWidthChars <- getWord8
  screenWidth <- getWord16be
  screenHeight <- getWord16be

  -- Field order depends on version
  (fontWidth, fontHeight) <- let parseFont v | v < 5 = do skip 2
                                                          return (0, 0)
                                 parseFont v | v == 5 = do w <- getWord8
                                                           h <- getWord8
                                                           return (w, h)
                                 parseFont _ = do h <- getWord8
                                                  w <- getWord8
                                                  return (w, h)
                                 in parseFont version

  routinesOffset <- getWord16be
  staticStringsOffset <- getWord16be
  backgroundColour <- getWord8
  foregroundColour <- getWord8
  endCharacterTable <- getWord16be
  stream3OutputPixels <- getWord16be
  revisionNumber <- getWord16be
  alphabetTable <- getWord16be
  extensionTable <- getWord16be


  let fileLength = scale (fromIntegral rawFileLength) where
        scale :: Word32 -> Word32
        scale x | version < 4 = x * 2
        scale x | version < 6 = x * 4
        scale x = x * 8

  return $ Header { .. }

showHeader :: Header -> TL.Text
showHeader header = TB.toLazyText $ mconcat [ bprint "Z-code version" version F.int
                                            , bprint "Interpreter flags" flags1 F.int
                                            , bprint "Release number" releaseNumber F.int
                                            , bprint "Size of resident memory" baseHighMemory F.hex
                                            , bprint "Start PC" initPC F.hex
                                            , bprint "Dictionary address" dictionary F.hex
                                            , bprint "Object table address" objectTable F.hex
                                            , bprint "Global variables address" variablesTable F.hex
                                            , bprint "Size of dynamic memory" baseStaticMemory F.hex
                                            , bprint "Game flags" flags2 F.int
                                            , F.bprint (field F.text) "Serial number" (TL.fromStrict . TE.decodeUtf8 $
                                                                                       serialCode header)
                                            , bprint "Abbreviations address" abbreviationsTable F.hex
                                            , bprint "File size" fileLength F.hex
                                            , bprint "Checksum" checksum F.hex
                                            -- TODO : Terminating keys
                                            -- TODO : Header extension
                                            , bprint "Inform Version" interpreterNumber F.int
                                            ]
  where
    field typ = (F.right 26 ' ' %. F.text % ": ") % typ % "\n"
    bprint fieldName accessor fieldType   = F.bprint (field fieldType) fieldName (accessor header)
