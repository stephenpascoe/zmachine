{-

Functions here may throw ZsciiException on data that cannot be handled.

-}
module Language.ZMachine.Dictionary
  ( HasDictionary(..)
  , Dictionary(..)
    --
  , decodeWordEntries
  ) where

import RIO

import qualified RIO.Text as T
import Data.Binary.Get

import qualified Language.ZMachine.Memory as M
import qualified Language.ZMachine.ZSCII as Z
import Language.ZMachine.App (App)
import Language.ZMachine.Types


class HasDictionary env where
  getDictionary :: M.HasMemory env => RIO env Dictionary

instance HasDictionary App where
  getDictionary = do header <- M.getHeader
                     let dictOffset = dictionaryOffset header
                         version = zVersion header
                         aTable = Nothing
                     stream <- M.streamBytes (fromIntegral dictOffset)
                     return $ runGet (decodeDictionary version aTable) stream


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


instance Display Dictionary where
  display dict = mconcat entryBs where
    buildEntry :: (Integer, ZsciiString) -> Utf8Builder
    buildEntry (i, zseq) = "[" <> (display i) <> "] " <> (display (Z.zseqToText zseq)) <> "\n"
    entryBs = map buildEntry $ zip [0..] (entries dict)
