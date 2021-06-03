{- ZChars encode 3 chars into 2 bytes.

-- Ending a string early

3.6

Since the end-bit only comes up once every three Z-characters, a string may have to be
'padded out' with null values. This is conventionally achieved with a sequence of 5's, though
a sequence of (for example) 4's would work equally well.

-}
module Language.ZMachine.ZSCII.ZChars
    ( zstrToZchars
    , zcharsToZstr
    , ZString
    , ZsciiString(..)
    , ZChar
    , AbbreviationTable
  -- Internal
    , packZchars
    , unpackZchars
    , hasStopBit
    )
where

import           RIO

import qualified RIO.ByteString.Lazy           as BL
import qualified RIO.ByteString                as B
import qualified RIO.Vector.Boxed              as V
import           Data.Bits
import           Data.Binary.Put
import           Data.Binary.Get
import           Data.Binary


{-  TODO : Refactor thoughts

ZString is a redundant type which should be replaced with lazy bytestrings from HasMemory.
We always want to convert to ZsciiString eventually and don't always know how many bytes we need
to read from memory before we finish constructing the ZsciiString

-}
{-

We need to represent 4 different forms of character data

 1. The on-disk encoding of strings as documented in the spec.  This is represented
    as Lazy Bytestrings
 2. A sequence of ZChars.  A ZChar is a integer between 0 and 31.  ZChars are respresented
    as lists of Word8
 3. ZsciiString.  A byte encoding similar to latin1 referred to as ZSCII chars in the spec.  Represented as a newtype of ByteString.
 4. Text.  For final output.

-}

-- | Characters
type ZChar = Word8

-- Memory stream used to read Zscii data
type ZString = BL.ByteString

-- | A ByteString representing a decoded ZString into a squence of Zscii charaters
newtype ZsciiString = ZsciiString { unZsciiString :: B.ByteString } deriving (Show, Eq, IsString)

type AbbreviationTable = V.Vector ZsciiString


paddingChar :: ZChar
paddingChar = 5

-- | Parse a ByteString to a stream of ZChars
--   If there are an odd number of bytes in the bytestring the last byte will be discarded.
--   Reads the ByteString until a stop-bit occurs or until the end.

zstrToZchars :: ZString -> [ZChar]
zstrToZchars = runGet decodeZchars

zcharsToZstr :: [ZChar] -> ZString
zcharsToZstr zchars = runPut (encodeZchars zchars)


decodeZchars :: Get [ZChar]
decodeZchars = decodeWords <|> return []  where
    decodeWords = do
        w <- getWord16be
        let (z1, z2, z3) = unpackZchars w
        if hasStopBit w
            then return [z1, z2, z3]
            else do
                zchars <- decodeZchars
                return (z1 : z2 : z3 : zchars)


encodeZchars :: [ZChar] -> Put
encodeZchars [z1, z2, z3] = do
    let word = addStopBit $ packZchars (z1, z2, z3)
    put word

encodeZchars [z1, z2] = do
    let word = addStopBit $ packZchars (z1, z2, paddingChar)
    put word

encodeZchars [z1] = do
    let word = addStopBit $ packZchars (z1, paddingChar, paddingChar)
    put word
encodeZchars (z1 : z2 : z3 : rest) = do
    let word = packZchars (z1, z2, z3)
    put word
    encodeZchars rest
encodeZchars [] = return ()

hasStopBit :: Word16 -> Bool
hasStopBit w = w .&. 0x8000 == 0x8000

addStopBit :: Word16 -> Word16
addStopBit w = w .|. 0x8000


unpackZchars :: Word16 -> (ZChar, ZChar, ZChar)
unpackZchars w = (z1, z2, z3)  where
    mask' = 0x001f
    w'    = w .&. 0x7fff
    z1    = fromIntegral $ w' `shift` (-10) .&. mask'
    z2    = fromIntegral $ w' `shift` (-5) .&. mask'
    z3    = fromIntegral $ w' .&. mask'


packZchars :: (ZChar, ZChar, ZChar) -> Word16
packZchars (z1, z2, z3) =
    fromIntegral z3 .|. fromIntegral z2 `shift` 5 .|. fromIntegral z1 `shift` 10
