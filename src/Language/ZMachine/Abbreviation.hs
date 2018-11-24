
module Language.ZMachine.Abbreviation
  ( getAbbreviation
  , abbreviationTable
  ) where


import qualified Data.Vector as V
import Data.Vector ((!?))
import Data.Int
import Data.Word
import Data.Binary
import Data.Binary.Get

import Language.ZMachine.Types
import qualified Language.ZMachine.Memory as M

-- Abreviations
{-
If a is the abbreviation character and b is the next character, then
these point to abbreviation Eq(a-1)*32+b. A table of abbreviations is
stored in memory (usually in RAM) beginning at the byte address stored
in the header word at $18. This is a contiguous list of 32 (in V2) or
96 (in V3+) words, which are the word addresses where the abbreviation
Z-strings are stored.

I.e.
abbrevations are not fixed length.  Will need to implement the stop bit.
Not same format as dictionary.  Table of pointers.

-}

-- TODO : Replace error with exception monad
getAbbreviation :: Maybe AbbreviationTable -> Int8 -> Word8 -> ZsciiString
getAbbreviation Nothing _ _ = error "No abbreviations available"
getAbbreviation (Just t) a b = case t !? (fromIntegral (a * 32) + fromIntegral b) of
                                 Nothing -> error "Abbreviation index out of range"
                                 Just x -> x


abbreviationTable :: M.Handle -> AbbreviationTable
abbreviationTable h = let header = M.getHeader h
                          aTableOffset = abbreviationTableOffset header
                          version = zVersion header
                          aTableSize | version == 1 = 0
                                     | version == 2 = 32
                                     | otherwise    = 96
                      in
                        runGet (decodeAbbreviationTable aTableSize) (M.streamStoryBytes h (fromIntegral aTableOffset))
decodeAbbreviationTable :: Int -> Get AbbreviationTable
decodeAbbreviationTable = undefined
