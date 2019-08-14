module Language.ZMachine.Abbreviations
  ( abbreviations
  ) where

import RIO

import Data.Binary.Get
import Data.List

import qualified Language.ZMachine.Memory as M
import Language.ZMachine.Types

abbreviations :: M.HasMemory env => env -> AbbreviationTable
abbreviations env = let header = M.getHeader env
                        tableOffset = abbreviationTableOffset header
                        tableLength = if zVersion header < 3 then 32 else 96
                        tableOffsets = runGet (decodeTableOffsets tableLength) (M.streamBytes env (fromIntegral tableOffset))
                  in

                    -- TODO : Read ZString from each offset
                    -- TODO : Convert to vector
                    undefined

decodeTableOffsets :: Int -> Get [Word16]
decodeTableOffsets n = sequenceA $ take n $ repeat getWord16be
