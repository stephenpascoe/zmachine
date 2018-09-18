{-# LANGUAGE OverloadedStrings #-}

module Lib.ZSCII ( decode
                 , ZString(..)
                 , ZSeq(..)
                 , ZSCII
                 , Version
                 , zseqToText
                 -- For debugging
                 , ZChar
                 , packZchars
                 , unpackZchars
                 )
where

import Data.Char
import Data.Word
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BB
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import Data.Monoid
import Data.Binary.Get
import Data.Bits
import Data.List
import Data.Int


-- TODO : ZSCII strings should probably be based on ByteString not Text.  Need final conversion
--        to text.

-- | Characters
type ZSCII = Word8
type ZChar = Word8
type Version = Int8

-- | A ByteString representing ZString encoded characters
newtype ZString = ZString B.ByteString deriving Show
-- | A ByteString representing a decoded ZString into a squence of ZSCII charaters
newtype ZSeq = ZSeq B.ByteString deriving Show

data Alphabet = Alpha0 | Alpha1 | Alpha2

-- | ZCharEvent models the interpretation of each ZChar in a ZChar stream.
data ZCharEvent = ShiftUpEvent Bool
                | ShiftDownEvent Bool
                | Abrev1Event
                | Abrev2Event
                | Abrev3Event
                | ZSCIIEvent ZSCII

-- | ZCharState models the state machine used for converting ZCars to ZSCII

data AbrevState = NoAbrevState | Abrev1State | Abrev2State | Abrev3State
data ParseState = ParseState { currentVersion :: Version
                             , currentAlphabet :: Alphabet
                             , nextCharAlphabet :: Maybe Alphabet
                             , abrevState :: AbrevState
                             , parsedChars :: BB.Builder
                             }


shiftUp Alpha0 = Alpha1
shiftUp Alpha1 = Alpha2
shiftUp Alpha2 = Alpha0
shiftDown Alpha0 = Alpha2
shiftDown Alpha1 = Alpha0
shiftDown Alpha2 = Alpha1


decode :: Version -> ZString -> ZSeq
decode version (ZString str) = ZSeq $ BL.toStrict . BB.toLazyByteString . parsedChars $ state where
  init = ParseState { currentVersion = version
                    , currentAlphabet = Alpha0
                    , abrevState = NoAbrevState
                    , nextCharAlphabet = Nothing
                    , parsedChars = mempty
                    }
  state = foldl' consumeByte init $ runGet decodeZchars (BL.fromStrict str)


zseqToText :: ZSeq -> T.Text
zseqToText (ZSeq bstr) = TE.decodeLatin1 bstr

consumeByte :: ParseState -> ZChar -> ParseState
consumeByte state char =
  let
    alpha = currentAlphabet state
    shift shiftF lock = state { currentAlphabet = shiftF alpha
                              , nextCharAlphabet = if lock then Nothing else Just alpha
                              }
  in
    case byteToEvent (currentVersion state) alpha char of
      ZSCIIEvent z -> state { parsedChars = parsedChars state <> BB.word8 z
                            , currentAlphabet = case nextCharAlphabet state of
                                                  Nothing -> alpha
                                                  Just a -> a
                            , nextCharAlphabet = Nothing
                            }
      ShiftUpEvent lock -> shift shiftUp lock
      ShiftDownEvent lock -> shift shiftDown lock

      Abrev1Event -> undefined
      Abrev2Event -> undefined
      Abrev3Event -> undefined



getAlphabetTable :: Version -> Alphabet -> ZSeq
getAlphabetTable 1 Alpha0 = ZSeq "abcdefghijklmnopqrstuvwxyz"
getAlphabetTable 1 Alpha1 = ZSeq "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
getAlphabetTable 1 Alpha2 = ZSeq "^0123456789.,!?_#'\"/\\<-:()"
getAlphabetTable _ Alpha0 = ZSeq "abcdefghijklmnopqrstuvwxyz"
getAlphabetTable _ Alpha1 = ZSeq "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
getAlphabetTable _ Alpha2 = ZSeq "^\n0123456789.,!?_#'\"/\\-:()"

byteToEvent :: Version -> Alphabet -> ZChar -> ZCharEvent
byteToEvent version _ char
  | char < 6 = byteToEvent' version char
byteToEvent version alphabet char
  | char < 32 = let ZSeq txt = getAlphabetTable version alphabet
                in  ZSCIIEvent $ B.index txt (fromIntegral (char - 6))

byteToEvent' :: Version -> ZChar -> ZCharEvent
byteToEvent' _ 0 = ZSCIIEvent $ (toEnum . fromEnum) ' '
byteToEvent' 1 1 = ZSCIIEvent $ (toEnum . fromEnum) '\n'
byteToEvent' 2 1 = Abrev1Event
byteToEvent' version char | version < 3 = case char of
                                            2 -> ShiftUpEvent False
                                            3 -> ShiftDownEvent False
                                            4 -> ShiftDownEvent True
                                            5 -> ShiftDownEvent True
byteToEvent' version char = if (char == 4) || (char == 5)
                            then case char of
                                   4 -> ShiftUpEvent False
                                   5 -> ShiftDownEvent False
                            else case char of
                                   1 -> Abrev1Event
                                   2 -> Abrev2Event
                                   3 -> Abrev3Event




--
-- Decoding ByteStrings to ZChars
--


-- | Parse a ByteString to a stream of ZChars
--   If mLength is provided parses that number of bytes, otherwise parse until stop bit is set.
decodeZchars :: Get [ZChar]
decodeZchars = do
  empty <- isEmpty
  if empty then return []
    else do w <- getWord16be
            zchars <- decodeZchars
            let (z1, z2, z3) = unpackZchars w
            return (z1:z2:z3:zchars)

unpackZchars :: Word16 -> (ZChar, ZChar, ZChar)
unpackZchars w = (z1, z2, z3) where
  mask = 0x001f
  w' = w .&. 0x7fff
  z1 = fromIntegral $ (w' `shift` (-10)) .&. mask
  z2 = fromIntegral $ (w' `shift`  (-5)) .&. mask
  z3 = fromIntegral $ w' .&. mask


packZchars :: (ZChar, ZChar, ZChar) -> Word16
packZchars (z1, z2, z3) = (fromIntegral z3) .|. ((fromIntegral z2) `shift` 5) .|. ((fromIntegral z1) `shift` 10)
