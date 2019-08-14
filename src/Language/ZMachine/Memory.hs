module Language.ZMachine.Memory
  ( HasMemory(..)
  ) where

import RIO hiding (Handle)

import qualified RIO.ByteString as B
import qualified RIO.ByteString.Lazy as BL
import qualified RIO.Text as T
import qualified Numeric as N
import Data.Binary.Get

import Language.ZMachine.Types
import Language.ZMachine.App

-- newtype Handle = Handle { storyBytes :: B.ByteString }

{-
new :: FilePath -> IO Handle
new path = do file <- BL.readFile path
              return $ Handle (BL.toStrict file)

close :: Handle -> IO ()
close _ = return ()
-}

-- HasMemory defines the interface to raw memory

class HasMemory env where
  getHeader :: RIO env Header
  getBytes :: Int -- ^ Offset
           -> Int -- ^ Length
           -> RIO env B.ByteString
  streamBytes :: Int -- ^ Offset
              -> RIO env BL.ByteString

instance HasMemory App where
  getHeader = do env <- ask
                 return $ runGet parseHeader (BL.fromStrict $ story env)
  getBytes offset n = do env <- ask
                         return $ B.take n $ B.drop offset (story env)
  streamBytes offset = do env <- ask
                          return $ BL.fromStrict $ B.drop offset (story env)


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


instance Display Header where
  display header = mconcat [ dEntry "Z-code version" zVersion
                           , dEntry "Interpreter flags" flags1
                           , dEntry "Release number" releaseNumber
                           , hexEntry "Size of resident memory" baseHighMemory
                           , hexEntry "Start PC" initPC
                           , hexEntry "Dictionary address" dictionaryOffset
                           , hexEntry "Object table address" objectTable
                           , hexEntry "Global variables address" variablesTable
                           , hexEntry "Size of dynamic memory" baseStaticMemory
                           , dEntry "Game flags" flags2
                           , dEntry "Serial number" ((T.decodeUtf8With T.lenientDecode) . serialCode)
                           , hexEntry "Abbreviations address" abbreviationTableOffset
                           , dEntry "File size" fileLength
                           , hexEntry "Checksum" checksum
                             -- TODO : Terminating keys
                             -- TODO : Header extension
                           , dEntry "Inform Version" interpreterNumber
                           ]
    where
      -- entry :: Display a => Text -> (Header -> a) -> Utf8Builder
      -- entry fieldName accessor =  (display fieldName) <> ": " <> (display (accessor header)) <> "\n"
      entry :: Display a => Text -> a -> Utf8Builder
      entry name value = (display name) <> ": " <> (display value) <> "\n"
      hexEntry :: Text -> (Header -> Word16) -> Utf8Builder
      hexEntry name accessor = entry name (T.pack $ (N.showHex (accessor header)) "")
      dEntry :: Display a => Text -> (Header -> a) -> Utf8Builder
      dEntry name accessor = entry name (accessor header)
