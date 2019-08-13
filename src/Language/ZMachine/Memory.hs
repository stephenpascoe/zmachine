module Language.ZMachine.Memory
  ( Handle
  , new
  , close
  , getHeader
  , getStoryBytes
  , streamStoryBytes
  , showHeader
  ) where

import RIO hiding (Handle)

import qualified RIO.ByteString as B
import qualified RIO.ByteString.Lazy as BL
import qualified RIO.Text as T
import Data.Binary.Get

import Language.ZMachine.Types

newtype Handle = Handle { storyBytes :: B.ByteString }


new :: FilePath -> IO Handle
new path = do file <- BL.readFile path
              return $ Handle (BL.toStrict file)

close :: Handle -> IO ()
close _ = return ()

getStoryBytes :: Handle
         -> Int -- ^ Offset
         -> Int -- ^ Length
         -> B.ByteString
getStoryBytes h offset n = B.take n $ B.drop offset (storyBytes h)

streamStoryBytes :: Handle
               -> Int -- ^ Offset
               -> BL.ByteString
streamStoryBytes h offset = BL.fromStrict $ B.drop offset (storyBytes h)

getHeader :: Handle -> Header
getHeader h = runGet parseHeader (BL.fromStrict $ storyBytes h)

parseHeader :: Get Header
parseHeader = do
  zVersion <- getInt8
  flags1 <- getWord8
  releaseNumber <- getWord16be
  baseHighMemory <- getWord16be
  initPC <- getWord16be
  dictionaryOffset <- getWord16be
  objectTable <- getWord16be
  variablesTable <- getWord16be
  baseStaticMemory <- getWord16be
  flags2 <- getWord8
  skip 1
  serialCode <- getByteString 6
  abbreviationTableOffset <- getWord16be
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
                                 in parseFont zVersion

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
        scale x | zVersion < 4 = x * 2
        scale x | zVersion < 6 = x * 4
        scale x = x * 8

  return $ Header { .. }

-- TODO : Display Hex
showHeader :: Header -> T.Text
showHeader header = textDisplay $ mconcat [ entry "Z-code version" zVersion
                                          , entry "Interpreter flags" flags1
                                          , entry "Release number" releaseNumber
                                          , entry "Size of resident memory" baseHighMemory
                                          , entry "Start PC" initPC
                                          , entry "Dictionary address" dictionaryOffset
                                          , entry "Object table address" objectTable
                                          , entry "Global variables address" variablesTable
                                          , entry "Size of dynamic memory" baseStaticMemory
                                          , entry "Game flags" flags2
                                          , entry "Serial number" ((T.decodeUtf8With T.lenientDecode) . serialCode)
                                          , entry "Abbreviations address" abbreviationTableOffset
                                          , entry "File size" fileLength
                                          , entry "Checksum" checksum
                                            -- TODO : Terminating keys
                                            -- TODO : Header extension
                                          , entry "Inform Version" interpreterNumber
                                          ]
  where
    entry :: Display a => Text -> (Header -> a) -> Utf8Builder
    entry fieldName accessor =  (display fieldName) <> ": " <> (display (accessor header)) <> "\n"
