module Language.ZMachine.Dictionary
  ( dictionary
  , DictionaryHeader(..)
  , Dictionary(..)
  , showDictionary
    --
  , decodeWordEntries
  ) where

import RIO

import Data.Binary.Get

import qualified Language.ZMachine.Memory as M
import qualified Language.ZMachine.ZSCII as Z
import Language.ZMachine.Types


dictionary :: M.HasMemory env => env -> Dictionary
dictionary env = let header = M.getHeader env
                     dictOffset = dictionaryOffset header
                     version = zVersion header
                     aTable = Nothing
               in
                 runGet (decodeDictionary version aTable) (M.streamBytes env (fromIntegral dictOffset))


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


showDictionary :: Dictionary -> Utf8Builder
showDictionary dict = mconcat entryBs where
  buildEntry :: (Integer, ZsciiString) -> Utf8Builder
  buildEntry (i, zseq) = "[" <> (display i) <> "] " <> (display (Z.zseqToText zseq)) <> "\n"
  entryBs = map buildEntry $ zip [0..] (entries dict)
