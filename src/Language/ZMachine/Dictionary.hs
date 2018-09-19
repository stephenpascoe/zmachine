{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Language.ZMachine.Dictionary
  ( dictionary
  , DictionaryHeader(..)
  , Dictionary(..)
  , showDictionary
    --
  , decodeWordEntries
  ) where

import Data.Binary.Get
import qualified Data.ByteString as B

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import qualified Formatting as F
import Formatting ((%))

import qualified Language.ZMachine.Memory as M
import qualified Language.ZMachine.ZSCII as Z


data DictionaryHeader = DictionaryHeader { inputCodes :: B.ByteString
                                         , entryLength :: Int
                                         , numEntries :: Int
                                         } deriving Show

data Dictionary = Dictionary { header :: DictionaryHeader
                             , entries :: [Z.ZSeq]
                             } deriving Show



dictionary :: M.Handle -> Dictionary
dictionary h = let header = M.getHeader h
                   dictOffset = M.dictionary header
                   version = M.version header
               in
                 runGet (decodeDictionary version) (M.streamStoryBytes h (fromIntegral dictOffset))


decodeDictionary :: Z.Version -> Get Dictionary
decodeDictionary v = do header <- decodeDictionaryHeader
                        -- Two bytes encodes 3 characters
                        let entryBytes = ceiling $ (fromIntegral $ entryLength header) * 2 / 3
                            totalBytes = entryBytes * (numEntries header)
                        entries <- decodeWordEntries v header
                        return $ Dictionary header entries


decodeDictionaryHeader :: Get DictionaryHeader
decodeDictionaryHeader = do n <- getWord8
                            inputCodes <- getByteString $ fromIntegral n
                            entryLength <- getWord8
                            numEntries <- getWord16be
                            return $ DictionaryHeader { inputCodes
                                                      , entryLength = fromIntegral entryLength
                                                      , numEntries = fromIntegral numEntries
                                                      }


-- TODO : use higher-order function instead of recursion
decodeWordEntries :: Z.Version -> DictionaryHeader -> Get [Z.ZSeq]
decodeWordEntries v h = let nmax = numEntries h
                            zlen = if v < 4 then 4 else 6
                            f n | n >= nmax = return []
                            f n = do zstr <- Z.ZString <$> getByteString zlen
                                     let entry = Z.decode v zstr
                                     skip $ (entryLength h) - zlen
                                     rest <- f (n+1)
                                     return $ entry:rest
                        in f 0


showDictionary :: Dictionary -> TL.Text
showDictionary dict = TB.toLazyText $ mconcat entryBs where
  buildEntry (i, zseq) = F.bprint ("[" % F.int % "] " % F.stext % "\n") i (Z.zseqToText zseq)
  entryBs = map buildEntry $ zip [0..] (entries dict)


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
