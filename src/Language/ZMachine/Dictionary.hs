{-

Functions here may throw ZsciiException on data that cannot be handled.

-}
module Language.ZMachine.Dictionary
  ( HasDictionary(..)
  , Dictionary(..)
  , DictionaryHeader(..)
    --
  , decodeWordEntries
  ) where

import RIO

import Data.Binary.Get
import qualified RIO.ByteString as B

import qualified Language.ZMachine.Memory as M
import qualified Language.ZMachine.ZSCII as Z
import Language.ZMachine.App (App)



class HasDictionary env where
  getDictionary :: M.HasMemory env => RIO env Dictionary

instance HasDictionary App where
  getDictionary = do header <- M.getHeader
                     let dictOffset = M.dictionaryOffset header
                         version = M.zVersion header
                         aTable = Nothing
                     stream <- M.streamBytes (fromIntegral dictOffset)
                     return $ runGet (decodeDictionary version aTable) stream



data DictionaryHeader = DictionaryHeader { inputCodes :: !B.ByteString
                                         , entryLength :: !Int
                                         , numEntries :: !Int
                                         } deriving Show

data Dictionary = Dictionary { header :: !DictionaryHeader
                             , entries :: ![Z.ZsciiString]
                             } deriving Show



decodeDictionary :: M.ZVersion -> Maybe Z.AbbreviationTable -> Get Dictionary
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


decodeWordEntries :: M.ZVersion -> DictionaryHeader -> Get [Z.ZString]
decodeWordEntries v h = let nmax = numEntries h
                            zlen = if (M.zVersionToInt v) < 4 then 4 else 6
                            f n | n >= nmax = return []
                            f n = do zstr <- Z.ZString <$> getByteString zlen
                                     skip $ (entryLength h) - zlen
                                     rest <- f (n+1)
                                     return $ zstr:rest
                        in f 0


instance Display Dictionary where
  display dict = mconcat entryBs where
    buildEntry :: (Integer, Z.ZsciiString) -> Utf8Builder
    buildEntry (i, zseq) = "[" <> (display i) <> "] " <> (display (Z.zseqToText zseq)) <> "\n"
    entryBs = map buildEntry $ zip [0..] (entries dict)
