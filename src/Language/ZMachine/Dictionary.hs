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


data ZsciiException = ZsciiException T.Text deriving (Show, Typeable)
instance Exception ZsciiException

class HasDictionary env where
  getDictionary :: M.HasMemory env => RIO env Dictionary

instance HasDictionary App where
  getDictionary = do env <- ask
                     let header = M.getHeader env
                         dictOffset = dictionaryOffset header
                         version = zVersion header
                         aTable = Nothing
                       in case runGet (decodeDictionary version aTable)
                                      (M.streamBytes env (fromIntegral dictOffset)) of
                            Left err -> throwIO $ ZsciiException err
                            Right dict -> return dict


decodeDictionary :: Version -> Maybe AbbreviationTable -> Get (Either T.Text Dictionary)
decodeDictionary version aTable = do
  dHeader <- decodeDictionaryHeader
  rawEntries <- decodeWordEntries version dHeader
  let eEntries = sequenceA $ fmap  (Z.decodeZString version aTable) rawEntries
  return $ case eEntries of
             Left err -> Left err
             Right entries -> Right $ Dictionary dHeader entries

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
