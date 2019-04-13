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
import Data.Int

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import qualified Formatting as F
import Formatting ((%))

import qualified Language.ZMachine.Memory as M
import qualified Language.ZMachine.ZSCII as Z
import Language.ZMachine.Types


dictionary :: M.Handle -> Dictionary
dictionary h = let header = M.getHeader h
                   dictOffset = dictionaryOffset header
                   version = zVersion header
                   aTable = Nothing
               in
                 runGet (decodeDictionary version aTable) (M.streamStoryBytes h (fromIntegral dictOffset))


decodeDictionary :: Version -> Maybe AbbreviationTable -> Get Dictionary
decodeDictionary version aTable = do
  dHeader <- decodeDictionaryHeader
  rawEntries <- decodeWordEntries version dHeader
  let entries = fmap (Z.decodeZString version aTable) rawEntries
  return $ Dictionary dHeader entries

decodeDictionaryHeader :: Get DictionaryHeader
decodeDictionaryHeader = do n <- getWord8
                            inputCodes <- getByteString $ fromIntegral n
                            entryLength <- getWord8
                            numEntries <- getWord16be
                            return $ DictionaryHeader { inputCodes
                                                      , entryLength = fromIntegral entryLength
                                                      , numEntries = fromIntegral numEntries
                                                      }


decodeWordEntries :: Version -> DictionaryHeader -> Get [ZString]
decodeWordEntries v h = let nmax = numEntries h
                            zlen = if v < 4 then 4 else 6
                            f n | n >= nmax = return []
                            f n = do zstr <- ZString <$> getByteString zlen
                                     skip $ (entryLength h) - zlen
                                     rest <- f (n+1)
                                     return $ zstr:rest
                        in f 0


showDictionary :: Dictionary -> TL.Text
showDictionary dict = TB.toLazyText $ mconcat entryBs where
  buildEntry :: (Integer, ZsciiString) -> TB.Builder
  buildEntry (i, zseq) = F.bprint ("[" % F.int % "] " % F.stext % "\n") i (Z.zseqToText zseq)
  entryBs = map buildEntry $ zip [0..] (entries dict)
