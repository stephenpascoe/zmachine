{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Lib.Dictionary ( dictionary
                      , DictionaryHeader(..)
                      , Dictionary(..)
                      )
where

import Data.Binary.Get
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Data.Word

import qualified Lib.Memory as M
import qualified Lib.ZSCII as Z

data DictionaryHeader = DictionaryHeader { inputCodes :: B.ByteString
                                         , entryLength :: Int
                                         , numEntries :: Int
                                         } deriving Show

data Dictionary = Dictionary { header :: DictionaryHeader
                             , dictBytes :: B.ByteString
                             }



dictionary :: M.Handle -> Dictionary
dictionary h = let header = M.getHeader h
                   dictOffset = M.dictionary header
               in
                 runGet decodeDictionary (M.streamStoryBytes h (fromIntegral dictOffset))


decodeDictionary :: Get Dictionary
decodeDictionary = do header <- decodeDictionaryHeader
                      -- Two bytes encodes 3 characters
                      let entryBytes = ceiling $ (fromIntegral $ entryLength header) * 2 / 3
                          totalBytes = entryBytes * (numEntries header)
                      bytes <- getByteString totalBytes
                      return $ Dictionary header bytes


decodeDictionaryHeader :: Get DictionaryHeader
decodeDictionaryHeader = do n <- getWord8
                            inputCodes <- getByteString $ fromIntegral n
                            entryLength <- getWord8
                            numEntries <- getWord16be
                            return $ DictionaryHeader { inputCodes
                                                      , entryLength = fromIntegral entryLength
                                                      , numEntries = fromIntegral numEntries
                                                      }
{-
decodeWordEntry :: Z.Version -> DictionaryHeader -> Get Z.ZSeq
decodeWordEntry version header = do
  bytes <- getByteString (entryLength header)
  -- TODO : need to constrain parse to entryLength
  --        also rename decode at the same time
  return $ Z.decode version (Just (entryLength header)) (Z.ZString bytes)

decodeWordEntries :: Int -> Z.Version -> DictionaryHeader -> Get [Z.ZSeq]
decodeWordEntries n version header | n >= (numEntries header) = return []
decodeWordEntries n version header = do entry <- decodeWordEntry version header
                                        rest <- decodeWordEntries (n+1) version header
                                        return $ entry:rest
-}
