{-# LANGUAGE OverloadedStrings #-}

module Language.ZMachine.Abbreviations
  ( abbreviations
  ) where

import Data.Binary.Get
import Data.Word

import qualified Language.ZMachine.Memory as M
import Language.ZMachine.Types

abbreviations :: M.Handle -> AbbreviationTable
abbreviations h = let header = M.getHeader h
                      tableOffset = abbreviationTableOffset header
                      tableLength = if zVersion header < 3 then 32 else 96
                      tableOffsets = runGet (decodeTableOffsets tableLength) (M.streamStoryBytes h (fromIntegral tableOffset))
                  in

                    -- TODO : Read ZString from each offset
                    -- TODO : Convert to vector
                    undefined

decodeTableOffsets :: Int -> Get [Word16]
decodeTableOffsets n = sequenceA $ take n $ repeat getWord16be
