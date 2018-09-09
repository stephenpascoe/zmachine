{-# LANGUAGE OverloadedStrings #-}

module Lib.ZSCII ( parse
                 ,
                 )

where

import Data.Char
import Data.Word
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Builder as TB
import Data.Monoid
import Data.Binary.Get
import Data.Bits
import Data.List

-- | Characters
type ZSCII = Char
type ZChar = Word8
type Version = Int

-- | A ByteString representing ZString encoded characters
newtype ZString = ZString B.ByteString

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
data ParseState = ParseState { currentVersion :: Int
                             , currentAlphabet :: Alphabet
                             , nextCharAlphabet :: Maybe Alphabet
                             , abrevState :: AbrevState
                             , parsedChars :: TB.Builder
                             }


shiftUp Alpha0 = Alpha1
shiftUp Alpha1 = Alpha2
shiftUp Alpha2 = Alpha0
shiftDown Alpha0 = Alpha2
shiftDown Alpha1 = Alpha0
shiftDown Alpha2 = Alpha1

parse :: Version -> ZString -> T.Text
parse version (ZString str) = TB.toLazyText . parsedChars $ state where
  init = ParseState { currentVersion = version
                    , currentAlphabet = Alpha0
                    , abrevState = NoAbrevState
                    , nextCharAlphabet = Nothing
                    , parsedChars = mempty
                    }
  state = foldl' consumeByte init $ runGet parseZchars (BL.fromStrict str)


consumeByte :: ParseState -> ZChar -> ParseState
consumeByte state char =
  let
    alpha = currentAlphabet state
    shift shiftF lock = state { currentAlphabet = shiftF alpha
                              , nextCharAlphabet = if lock then Nothing else Just alpha
                              }
  in
    case byteToEvent (currentVersion state) alpha char of
      ZSCIIEvent z -> state { parsedChars = TB.singleton z <> parsedChars state
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



getAlphabetTable :: Version -> Alphabet -> T.Text
getAlphabetTable 1 Alpha0 = "abcdefghijklmnopqrstuvwxyz"
getAlphabetTable 1 Alpha1 = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
getAlphabetTable 1 Alpha2 = "^0123456789.,!?_#'\"/\\<-:()"
getAlphabetTable _ Alpha0 = "abcdefghijklmnopqrstuvwxyz"
getAlphabetTable _ Alpha1 = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
getAlphabetTable _ Alpha2 = "^\n0123456789.,!?_#'\"/\\-:()"

byteToEvent :: Version -> Alphabet -> ZChar -> ZCharEvent
byteToEvent version _ char
  | char < 6 = byteToEvent' version char
byteToEvent version alphabet char
  | char < 32 = let txt = getAlphabetTable version alphabet
                in  ZSCIIEvent $ T.index txt (fromIntegral char)

byteToEvent' :: Version -> ZChar -> ZCharEvent
byteToEvent' _ 0 = ZSCIIEvent ' '
byteToEvent' 1 1 = ZSCIIEvent '\n'
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
