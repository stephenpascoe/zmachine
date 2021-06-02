module Language.ZMachine.Abbreviations
  ( getAbbreviations
  ) where

import RIO

import Data.Binary.Get
import Data.List
import qualified RIO.Vector.Boxed              as V


import qualified Language.ZMachine.Memory as M
import Language.ZMachine.ZSCII (AbbreviationTable, ZsciiString(..))

getAbbreviations :: M.HasMemory env => RIO env AbbreviationTable
getAbbreviations = traverse readAbbreviation =<< getTableOffsets

getTableOffsets :: M.HasMemory env => RIO env (V.Vector Word16)
getTableOffsets = do header <- M.getHeader 
                     let tableOffset = M.abbreviationTableOffset header
                         tableLength = if M.zVersionToInt (M.zVersion header) < 3 then 32 else 96
                     stream <- M.streamBytes (fromIntegral tableOffset)
                     return $ runGet (V.replicateM tableLength getWord16be) stream

readAbbreviation :: M.HasMemory env => Word16 -> RIO env ZsciiString 
readAbbreviation tableOffset = 
  do stream <- M.streamBytes (fromIntegral tableOffset)
     return $ ZsciiString "x"