{-# LANGUAGE OverloadedStrings #-}

module Lib.ZSCII (
                 )

where

import Data.Char
import Data.Word
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import Data.Binary.Get
import Data.Bits


-- | Characters
type ZSCII = Char
type ZChar = Word8
type Version = Int

-- | A ByteString representing ZString encoded characters
newtype ZString = ZString B.ByteString

data Alphabet = Alpha0 | Alpha1 | Alpha2

-- | Alphabet represents characters from value 6 onwards
data AlphabetTable = AlphabetTable { alpha0 :: T.Text
                                   , alpha1 :: T.Text
                                   , alpha2 :: T.Text
                                   }

data AlphabetTableState = ShiftState Alphabet
                        | ShiftLockState Alphabet
                        | Abrev1State
                        | Abrev2State
                        | Abrev3State
                        | ZSCIIState ZSCII

alphabetV1 :: AlphabetTable
alphabetV1 = AlphabetTable { alpha0 = "abcdefghijklmnopqrstuvwxyz"
                           , alpha1 = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                           , alpha2 = "^0123456789.,!?_#'\"/\\<-:()"
                           }

alphabetV2 :: AlphabetTable
alphabetV2 = AlphabetTable { alpha0 = "abcdefghijklmnopqrstuvwxyz"
                           , alpha1 = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                           , alpha2 = "^\n0123456789.,!?_#'\"/\\-:()"
                           }

-- TODO : typesafe version
getAlphabetTable :: Version -> Alphabet -> T.Text
getAlphabetTable 1 Alpha0 = alpha0 alphabetV1
getAlphabetTable 1 Alpha1 = alpha1 alphabetV1
getAlphabetTable 1 Alpha2 = alpha2 alphabetV1
getAlphabetTable _ Alpha0 = alpha0 alphabetV2
getAlphabetTable _ Alpha1 = alpha1 alphabetV2
getAlphabetTable _ Alpha2 = alpha2 alphabetV2

byteToState :: Version -> Alphabet -> ZChar -> AlphabetTableState
byteToState version alphabet char
  | char < 6 = byteToState' version alphabet char
byteToState version alphabet char
  | char < 32 = let txt = getAlphabetTable version alphabet
                in  ZSCIIState $ T.index txt (fromIntegral char)


byteToState' :: Version -> Alphabet -> ZChar -> AlphabetTableState
byteToState' _ _ 0 = ZSCIIState ' '
byteToState' 1 _ 1 = ZSCIIState '\n'
byteToState' 2 _ 1 = undefined -- TODO : What is this?
byteToState' version alphabet char
  | version < 3 = byteToShiftStateV12 alphabet char
byteToState' version alphabet char = byteToStateV3 version alphabet char


byteToShiftStateV12 :: Alphabet -> ZChar -> AlphabetTableState
byteToShiftStateV12 Alpha0 2 = ShiftState Alpha1
byteToShiftStateV12 Alpha1 2 = ShiftState Alpha2
byteToShiftStateV12 Alpha2 2 = ShiftState Alpha0
byteToShiftStateV12 Alpha0 3 = ShiftState Alpha2
byteToShiftStateV12 Alpha1 3 = ShiftState Alpha0
byteToShiftStateV12 Alpha2 3 = ShiftState Alpha1
byteToShiftStateV12 Alpha0 4 = ShiftLockState Alpha1
byteToShiftStateV12 Alpha1 4 = ShiftLockState Alpha2
byteToShiftStateV12 Alpha2 4 = ShiftLockState Alpha0
byteToShiftStateV12 Alpha0 5 = ShiftLockState Alpha2
byteToShiftStateV12 Alpha1 5 = ShiftLockState Alpha0
byteToShiftStateV12 Alpha2 5 = ShiftLockState Alpha1

byteToStateV3 :: Version -> Alphabet -> ZChar -> AlphabetTableState
byteToStateV3 _ alphabet char
  | (char == 4) || (char == 5) = byteToShiftStateV3 alphabet char
byetToStateV3 = byteToAbrev

byteToShiftStateV3 :: Alphabet -> ZChar -> AlphabetTableState
byteToShiftStateV3 Alpha0 4 = ShiftState Alpha1
byteToShiftStateV3 Alpha1 4 = ShiftState Alpha2
byteToShiftStateV3 Alpha2 4 = ShiftState Alpha0
byteToShiftStateV3 Alpha0 5 = ShiftState Alpha2
byteToShiftStateV3 Alpha1 5 = ShiftState Alpha0
byteToShiftStateV3 Alpha2 5 = ShiftState Alpha1

-- TODO : byteToAbrev
byteToAbrev :: Version -> Alphabet -> ZChar -> AlphabetTableState
byteToAbrev = undefined



--
-- Decoding ByteStrings to ZChars
--

-- TODO : Optional stop conditions: end of string, fixed length or until ByteString exausted
-- | Parse a ByteString to a stream of ZChars
parseZchars :: Get [ZChar]
parseZchars = do
  empty <- isEmpty
  if empty
    then return []
    else do w <- getWord16be
            let (z1, z2, z3) = unpackZchars w
            -- End of Z-string
            if (w .&. 0x8000) == 0x8000
              then return [z1,z2,z3]
              else do zchars <- parseZchars
                      return (z1:z2:z3:zchars)


unpackZchars :: Word16 -> (ZChar, ZChar, ZChar)
unpackZchars w = (z1, z2, z3) where
  mask = 0x001f
  w' = w `xor` 0x8000
  z1 = fromIntegral $ (w' `shift` (-10)) .&. mask
  z2 = fromIntegral $ (w' `shift`  (-5)) .&. mask
  z3 = fromIntegral $ w' .&. mask
