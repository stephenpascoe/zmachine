module Language.ZMachine.Types
  ( Header(..)
  , DictionaryHeader(..)
  , Dictionary(..)
  , ZString(..)
  , ZsciiString(..)
  , Zscii
  , ZChar
  , AbbreviationTable
  , Version
  , Property(..)
  , Object(..)
  , ObjectTable(..)
  , ByteAddress
  ) where

import RIO
import qualified RIO.ByteString as B
import qualified RIO.Vector.Boxed as V

type ByteAddress = Word16
type Colour = Word8


-- TODO : Use real data types for flags
type Flags1 = Word8
type Flags2 = Word8

data Header = Header { zVersion :: !Int8
                     , flags1 :: !Flags1   -- Mutable
                     , releaseNumber :: !Word16
                     , baseHighMemory :: !ByteAddress
                     , initPC :: !Word16   -- Packed address in V6
                     , dictionaryOffset :: !ByteAddress
                     , objectTable :: !ByteAddress
                     , variablesTable :: !ByteAddress
                     , baseStaticMemory :: !ByteAddress
                     , flags2 :: !Flags2   -- Mutable
                     , serialCode :: !B.ByteString
                     , abbreviationTableOffset :: !ByteAddress
                     , fileLength :: !Word32
                     , checksum :: !Word16
                     , interpreterNumber :: !Word8
                     , interpreterVersion :: !Word8
                     , screenHeightLines :: !Word8
                     , screenWidthChars :: !Word8
                     , screenWidth :: !Word16 -- units or chars depending on version
                     , screenHeight :: !Word16
                     , fontWidth :: !Word8
                     , fontHeight :: !Word8
                     , routinesOffset :: !Word16
                     , staticStringsOffset :: !Word16
                     , backgroundColour :: !Colour -- Mutable
                     , foregroundColour :: !Colour -- Mutable
                     , endCharacterTable :: !ByteAddress
                     , stream3OutputPixels :: !Word16 -- Mutable
                     , revisionNumber :: !Word16 -- Mutable
                     , alphabetTable :: !ByteAddress
                     , extensionTable :: !ByteAddress
                     } deriving Show


data DictionaryHeader = DictionaryHeader { inputCodes :: !B.ByteString
                                         , entryLength :: !Int
                                         , numEntries :: !Int
                                         } deriving Show

data Dictionary = Dictionary { header :: !DictionaryHeader
                             , entries :: ![ZsciiString]
                             } deriving Show


{-

We need to represent 4 different forms of character data

 1. The on-disk encoding of strings as documented in the spec.  This is represented
    as a strict ByteString wrapped in the ZString newtype
 2. A sequence of ZChars.  A ZChar is a integer between 0 and 31.  ZChars are respresented
    as lists of Word8
 3. ZsciiString.  A byte encoding similar to latin1 referred to as ZSCII chars in the spec.  Represented as a newtype of ByteString.
 4. Text.  For final output.

-}


type AbbreviationTable = V.Vector ZsciiString
type Version = Int8

-- | Characters
type Zscii = Word8
type ZChar = Word8

-- | A ByteString representing ZString encoded characters
newtype ZString = ZString { unZString :: B.ByteString } deriving (Show, Eq)


-- | A ByteString representing a decoded ZString into a squence of Zscii charaters
newtype ZsciiString = ZsciiString { unZsciiString :: B.ByteString } deriving (Show, Eq)


-- | Properties are unstructured binary data
data Property = Property Int8 ByteString deriving Show

data Object = Object { attributes :: B.ByteString
                     , description :: ZsciiString
                     , parentId :: Integer
                     , siblingId :: Integer
                     , childId :: Integer
                     , properties :: [Property]
                     } deriving Show

type ObjectTable = V.Vector Object
