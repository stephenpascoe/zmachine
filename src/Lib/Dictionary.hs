{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Lib.Dictionary ( dictionary
                      , DictionaryHeader(..)
                      , Dictionary(..)
                      --
                      , decodeWordEntries
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
