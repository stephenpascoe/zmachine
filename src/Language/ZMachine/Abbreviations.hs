module Language.ZMachine.Abbreviations
    ( getAbbreviationTable
    , HasAbbreviations
    , displayAbbreviations
    )
where

import           RIO

import           Data.Binary.Get
import           Data.List
import qualified RIO.Vector.Boxed              as V
import qualified Language.ZMachine.ZSCII       as Z



import qualified Language.ZMachine.Memory      as M
import           Language.ZMachine.ZSCII        ( AbbreviationTable
                                                , ZsciiString
                                                , decodeZString
                                                )
import           Language.ZMachine.App          ( App )


class HasAbbreviations env where
  getAbbreviationTable :: M.HasMemory env => RIO env AbbreviationTable

instance HasAbbreviations App where
    getAbbreviationTable = traverse readAbbreviation =<< getTableOffsets


getTableOffsets :: M.HasMemory env => RIO env (V.Vector Word16)
getTableOffsets = do
    header <- M.getHeader
    let tableOffset = M.abbreviationTableOffset header
        tableLength =
            if M.zVersionToInt (M.zVersion header) < 3 then 32 else 96
    stream <- M.streamBytes (fromIntegral tableOffset)
    return $ runGet (V.replicateM tableLength getWord16be) stream

readAbbreviation :: M.HasMemory env => Word16 -> RIO env ZsciiString
readAbbreviation tableOffset = do
    header <- M.getHeader
    let version = M.zVersion header
    stream <- M.streamBytes (fromIntegral tableOffset)
    -- We assume abbreviations do not contain references to abbreviations
    return $ decodeZString version Nothing stream



displayAbbreviations :: AbbreviationTable -> Utf8Builder
displayAbbreviations aTable = mconcat
    $ zipWith (curry buildEntry) [0 ..] (V.toList aTable)  where
    buildEntry :: (Integer, Z.ZsciiString) -> Utf8Builder
    buildEntry (i, zseq) =
        "["
            <> display i
            <> "] "
            <> "|"
            <> display (Z.zseqToText zseq)
            <> "|"
            <> "\n"
