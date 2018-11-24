
module Language.ZMachine.Types
  ( DictionaryHeader(..)
  , Dictionary(..)
  , ZString(..)
  , ZsciiString(..)
  , ZChars(..)
  , Zscii
  , ZChar
  , AbbreviationTable
  , Version
  ) where

import qualified Data.ByteString as B
import Data.Word
import Data.Int
import Data.Binary
import qualified Data.Vector as V


data DictionaryHeader = DictionaryHeader { inputCodes :: B.ByteString
                                         , entryLength :: Int
                                         , numEntries :: Int
                                         } deriving Show

data Dictionary = Dictionary { header :: DictionaryHeader
                             , entries :: [ZsciiString]
                             } deriving Show


{-

We need to represent 4 different forms of character data in this module

 1. The on-disk encoding of strings as documented in the spec.  This is represented
    as a strict ByteString wrapped in the ZString newtype
 2. A sequence of ZChars.  A ZChar is a integer between 0 and 31.  ZChars are respresented
    as lists of Word8
 3. ZsciiString.  A byte encoding similar to latin1.  Represented as a newtype of ByteString.
 4. Text.  For final output.

-}


type AbbreviationTable = V.Vector ZsciiString
type Version = Int8

-- | Characters
type Zscii = Word8
type ZChar = Word8

-- | A ByteString representing ZString encoded characters
newtype ZString = ZString { unZString :: B.ByteString } deriving Show
-- | A ByteString representing a decoded ZString into a squence of Zscii charaters
newtype ZsciiString = ZsciiString { unZsciiString :: B.ByteString } deriving Show
-- | A sequence of ZChars
newtype ZChars = ZChars [ZChar]
