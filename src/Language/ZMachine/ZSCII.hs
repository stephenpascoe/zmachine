{-# LANGUAGE OverloadedStrings #-}

module Language.ZMachine.ZSCII
  ( decodeZString
  , AbbreviationTable
  , ZString(..)
  , ZsciiString(..)
  , Zscii
  , zseqToText
  -- For debugging
  , ZChar
  , ZChars(..)
  , packZchars
  , unpackZchars
  , decodeZchars
  ) where

import Data.Word
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BB
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T

import Data.Binary
import Data.Binary.Get

import Data.Bits
import Data.List
import Data.Int
import Control.Applicative

import Language.ZMachine.Types
import Language.ZMachine.Abbreviation (getAbbreviation)


data Alphabet = Alpha0 | Alpha1 | Alpha2

-- | ZCharEvent models the interpretation of each ZChar in a ZChar stream.
data ZCharEvent = ShiftUpEvent Bool
                | ShiftDownEvent Bool
                | Abrev1Event
                | Abrev2Event
                | Abrev3Event
                | ZsciiEvent Zscii

-- | ZCharState models the state machine used for converting ZCars to Zscii

data AbrevState = NoAbrevState | Abrev1State | Abrev2State | Abrev3State
data ParseState = ParseState { alphabetState :: AlphabetState
                             , abrevState :: AbrevState
                             , parsedChars :: BB.Builder
                             }
data AlphabetState = AlphabetState { currentAlphabet :: Alphabet
                                   , nextAlphabet :: Maybe Alphabet
                                   }

shiftUp Alpha0 = Alpha1
shiftUp Alpha1 = Alpha2
shiftUp Alpha2 = Alpha0
shiftDown Alpha0 = Alpha2
shiftDown Alpha1 = Alpha0
shiftDown Alpha2 = Alpha1


zseqToText :: ZsciiString -> T.Text
zseqToText (ZsciiString bstr) = TE.decodeLatin1 bstr

getAlphabetTable :: Version -> Alphabet -> ZsciiString
getAlphabetTable 1 Alpha0 = ZsciiString "abcdefghijklmnopqrstuvwxyz"
getAlphabetTable 1 Alpha1 = ZsciiString "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
getAlphabetTable 1 Alpha2 = ZsciiString "^0123456789.,!?_#'\"/\\<-:()"
getAlphabetTable _ Alpha0 = ZsciiString "abcdefghijklmnopqrstuvwxyz"
getAlphabetTable _ Alpha1 = ZsciiString "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
getAlphabetTable _ Alpha2 = ZsciiString "^\n0123456789.,!?_#'\"/\\-:()"


decodeZString :: Version                 -- ^ ZMachine version
              -> Maybe AbbreviationTable -- ^ Abbreviations, if available
              -> ZString                 -- ^ Input ZString
              -> ZsciiString             -- ^ Resulting ZsciiString
decodeZString version aTable str =
  let init = ParseState { alphabetState = AlphabetState Alpha0 Nothing
                        , abrevState = NoAbrevState
                        , parsedChars = mempty
                        }

      alphabetTable = getAlphabetTable version

      ZChars bs = decodeZchars str
      finalState = foldl' consumeByte init bs

      consumeByte :: ParseState -> ZChar -> ParseState
      consumeByte state char =
        let alpha = currentAlphabet . alphabetState $ state
            shift shiftF lock = state { alphabetState = AlphabetState (shiftF alpha)
                                                                      (if lock then Nothing else Just alpha)
                                      }
        in
          case byteToEvent alpha char of
            ZsciiEvent z -> state { parsedChars = parsedChars state <> BB.word8 z
                                  , alphabetState = AlphabetState (case nextAlphabet . alphabetState $ state of
                                                                     Nothing -> alpha
                                                                     Just a -> a
                                                                  ) Nothing
                                  }
            ShiftUpEvent lock -> shift shiftUp lock
            ShiftDownEvent lock -> shift shiftDown lock

            -- TODO : parseChars should probably have type ZsciiString
            Abrev1Event -> state { abrevState = Abrev1State
                                 , parsedChars = parsedChars state <> (BB.byteString . unZsciiString . getAbbreviation aTable 1) char }
            Abrev2Event -> state { abrevState = Abrev2State
                                 , parsedChars = parsedChars state <> (BB.byteString . unZsciiString . getAbbreviation aTable 2) char }
            Abrev3Event -> state { abrevState = Abrev3State
                                 , parsedChars = parsedChars state <> (BB.byteString . unZsciiString . getAbbreviation aTable 3) char }


      byteToEvent :: Alphabet -> ZChar -> ZCharEvent
      byteToEvent _ 0 = ZsciiEvent $ (toEnum . fromEnum) ' '
      byteToEvent _ 1
        | version == 1 = ZsciiEvent $ (toEnum . fromEnum) '\n'
        | version == 2 = Abrev1Event
      byteToEvent _ char
        | char < 6 && version < 3 = case char of
                                      2 -> ShiftUpEvent False
                                      3 -> ShiftDownEvent False
                                      4 -> ShiftDownEvent True
                                      5 -> ShiftDownEvent True
      byteToEvent _ char
        | char < 6 = case char of
                       1 -> Abrev1Event
                       2 -> Abrev2Event
                       3 -> Abrev3Event
                       4 -> ShiftUpEvent False
                       5 -> ShiftDownEvent False

      byteToEvent alphabet char
        | char < 32 = let ZsciiString txt = alphabetTable alphabet
                      in
                        ZsciiEvent $ B.index txt (fromIntegral (char - 6))

  in
    ZsciiString $ BL.toStrict . BB.toLazyByteString . parsedChars $ finalState


--
-- Decoding ByteStrings to ZChars
--


-- | Parse a ByteString to a stream of ZChars
--   If there are an odd number of bytes in the bytestring the last byte will be discarded.
--   Reads the ByteString until a stop-bit occurs or until the end.

decodeZchars :: ZString -> ZChars
decodeZchars (ZString bs) = Data.Binary.decode (BL.fromStrict bs)

decodeZchars' ::  Get ZChars
decodeZchars' = decodeWords <|> return (ZChars []) where
  decodeWords = do w <- getWord16be
                   let (z1, z2, z3) = unpackZchars w
                   if hasStopBit w then return $ ZChars [z1, z2, z3] else do
                     ZChars zchars <- decodeZchars'
                     return $ ZChars (z1:z2:z3:zchars)

{-
encodeZchars :: ZChars -> ZString
encodeZchars = ZString . BL.toStrict . BB.toLazyByteString . encodeZchars'
-}

instance Binary ZChars where
  put = encodeZchars'
  get = decodeZchars'

encodeZchars :: ZChars -> ZString
encodeZchars zchars = ZString $ BL.toStrict $ Data.Binary.encode zchars

encodeZchars' :: ZChars -> Put
encodeZchars' (ZChars [z1, z2, z3]) = do
  let word = addStopBit $ packZchars (z1, z2, z3)
  put word

encodeZchars' (ZChars [z1, z2]) = do
  let word = addStopBit $ packZchars (z1, z2, 0)
  put word

encodeZchars' (ZChars [z1]) = do
  let word = addStopBit $ packZchars (z1, 0, 0)
  put word
encodeZchars' (ZChars (z1:z2:z3:rest)) = do
  let word = packZchars (z1, z2, z3)
  put word
  encodeZchars' $ ZChars rest
encodeZchars' (ZChars []) = return ()

hasStopBit :: Word16 -> Bool
hasStopBit w = w .&. 0x8000 == 0x8000

addStopBit :: Word16 -> Word16
addStopBit w = w .|. 0x8000

unpackZchars :: Word16 -> (ZChar, ZChar, ZChar)
unpackZchars w = (z1, z2, z3) where
  mask = 0x001f
  w' = w .&. 0x7fff
  z1 = fromIntegral $ (w' `shift` (-10)) .&. mask
  z2 = fromIntegral $ (w' `shift`  (-5)) .&. mask
  z3 = fromIntegral $ w' .&. mask


packZchars :: (ZChar, ZChar, ZChar) -> Word16
packZchars (z1, z2, z3) = (fromIntegral z3) .|. ((fromIntegral z2) `shift` 5) .|. ((fromIntegral z1) `shift` 10)
