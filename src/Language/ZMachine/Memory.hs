module Language.ZMachine.Memory
  ( HasMemory(..)
  , Header(..)
  , ByteAddress
  , ZVersion(..)
  , zVersionToInt
  , zVersionFromInt
  )
where

import           RIO                     hiding ( Handle )

import qualified RIO.ByteString                as B
import qualified RIO.ByteString.Lazy           as BL
import           Data.Binary.Get

import           Language.ZMachine.App

import qualified RIO.Text                      as T
import qualified Numeric                       as N


type ByteAddress = Word16
type Colour = Word8


-- TODO : Use real data types for flags
type Flags1 = Word8
type Flags2 = Word8

data ZVersion = ZVer1 | ZVer2 | ZVer3 | ZVer4
              | ZVer5 | ZVer6 | ZVer7 | ZVer8Plus deriving Show

instance Display ZVersion where
  display v = display $ zVersionToInt v

zVersionFromInt :: Int8 -> ZVersion
zVersionFromInt 1 = ZVer1
zVersionFromInt 2 = ZVer2
zVersionFromInt 3 = ZVer3
zVersionFromInt 4 = ZVer4
zVersionFromInt 5 = ZVer5
zVersionFromInt 6 = ZVer6
zVersionFromInt 7 = ZVer7
zVersionFromInt _ = ZVer8Plus

zVersionToInt :: ZVersion -> Int8
zVersionToInt ZVer1     = 1
zVersionToInt ZVer2     = 2
zVersionToInt ZVer3     = 3
zVersionToInt ZVer4     = 4
zVersionToInt ZVer5     = 5
zVersionToInt ZVer6     = 6
zVersionToInt ZVer7     = 7
zVersionToInt ZVer8Plus = 8


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
  getHeader = runGet parseHeader . BL.fromStrict . story <$> ask
  getBytes offset n = B.take n . B.drop offset . story <$> ask
  streamBytes offset = BL.fromStrict . B.drop offset . story <$> ask


parseHeader :: Get Header
parseHeader = do
  versionInt <- getInt8
  let zVersion = zVersionFromInt versionInt
  flags1           <- getWord8
  releaseNumber    <- getWord16be
  baseHighMemory   <- getWord16be
  initPC           <- getWord16be
  dictionaryOffset <- getWord16be
  objectTable      <- getWord16be
  variablesTable   <- getWord16be
  baseStaticMemory <- getWord16be
  flags2           <- getWord8
  skip 1
  serialCode              <- getByteString 6
  abbreviationTableOffset <- getWord16be
  rawFileLength           <- getWord16be
  checksum                <- getWord16be
  interpreterNumber       <- getWord8
  interpreterVersion      <- getWord8
  screenHeightLines       <- getWord8
  screenWidthChars        <- getWord8
  screenWidth             <- getWord16be
  screenHeight            <- getWord16be

  -- Field order depends on version
  (fontWidth, fontHeight) <-
    let parseFont v | v < 5 = do
          skip 2
          return (0, 0)
        parseFont v | v == 5 = do
          w <- getWord8
          h <- getWord8
          return (w, h)
        parseFont _ = do
          h <- getWord8
          w <- getWord8
          return (w, h)
    in  parseFont (zVersionToInt zVersion)

  routinesOffset      <- getWord16be
  staticStringsOffset <- getWord16be
  backgroundColour    <- getWord8
  foregroundColour    <- getWord8
  endCharacterTable   <- getWord16be
  stream3OutputPixels <- getWord16be
  revisionNumber      <- getWord16be
  alphabetTable       <- getWord16be
  extensionTable      <- getWord16be


  let fileLength = scale (fromIntegral rawFileLength)       where
        scale :: Word32 -> Word32
        scale x | (zVersionToInt zVersion) < 4 = x * 2
        scale x | (zVersionToInt zVersion) < 6 = x * 4
        scale x = x * 8

  return $ Header { .. }


data Header = Header
  { zVersion                :: !ZVersion
  , flags1                  :: !Flags1   -- Mutable
  , releaseNumber           :: !Word16
  , baseHighMemory          :: !ByteAddress
  , initPC                  :: !Word16   -- Packed address in V6
  , dictionaryOffset        :: !ByteAddress
  , objectTable             :: !ByteAddress
  , variablesTable          :: !ByteAddress
  , baseStaticMemory        :: !ByteAddress
  , flags2                  :: !Flags2   -- Mutable
  , serialCode              :: !B.ByteString
  , abbreviationTableOffset :: !ByteAddress
  , fileLength              :: !Word32
  , checksum                :: !Word16
  , interpreterNumber       :: !Word8
  , interpreterVersion      :: !Word8
  , screenHeightLines       :: !Word8
  , screenWidthChars        :: !Word8
  , screenWidth             :: !Word16 -- units or chars depending on version
  , screenHeight            :: !Word16
  , fontWidth               :: !Word8
  , fontHeight              :: !Word8
  , routinesOffset          :: !Word16
  , staticStringsOffset     :: !Word16
  , backgroundColour        :: !Colour -- Mutable
  , foregroundColour        :: !Colour -- Mutable
  , endCharacterTable       :: !ByteAddress
  , stream3OutputPixels     :: !Word16 -- Mutable
  , revisionNumber          :: !Word16 -- Mutable
  , alphabetTable           :: !ByteAddress
  , extensionTable          :: !ByteAddress
  }
  deriving Show


instance Display Header where
  display header = mconcat
    [ dEntry "Z-code version"    zVersion
    , dEntry "Interpreter flags" flags1
    , dEntry "Release number"    releaseNumber
    , hexEntry "Size of resident memory"  baseHighMemory
    , hexEntry "Start PC"                 initPC
    , hexEntry "Dictionary address"       dictionaryOffset
    , hexEntry "Object table address"     objectTable
    , hexEntry "Global variables address" variablesTable
    , hexEntry "Size of dynamic memory"   baseStaticMemory
    , dEntry "Game flags"    flags2
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
    hexEntry name accessor =
      entry name (T.pack $ (N.showHex (accessor header)) "")
    dEntry :: Display a => Text -> (Header -> a) -> Utf8Builder
    dEntry name accessor = entry name (accessor header)
