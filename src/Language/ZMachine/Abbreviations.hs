module Language.ZMachine.Abbreviations
  ( getAbbreviations
  ) where

import RIO

import Data.Binary.Get
import Data.List

import qualified Language.ZMachine.Memory as M
import Language.ZMachine.Types

getAbbreviations :: M.HasMemory env => RIO env AbbreviationTable
getAbbreviations = do header <- M.getHeader
                      let tableOffset = abbreviationTableOffset header
                          tableLength = if zVersion header < 3 then 32 else 96
                      stream <- M.streamBytes (fromIntegral tableOffset)
                      let tableOffsets = runGet (decodeTableOffsets tableLength) stream
                        in
                        -- TODO : Read ZString from each offset
                        -- TODO : Convert to vector
                        undefined

decodeTableOffsets :: Int -> Get [Word16]
decodeTableOffsets n = sequenceA $ take n $ repeat getWord16be
