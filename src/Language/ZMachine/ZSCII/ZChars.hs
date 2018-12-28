{-# LANGUAGE OverloadedStrings #-}

module Language.ZMachine.ZSCII.ZChars
 ( zstrToZchars
 , zcharsToZstr
 -- Internal
 , packZchars
 , unpackZchars
 ) where

import qualified Data.ByteString.Lazy as BL
import Data.Word
import Data.Bits
import Data.Binary
import Data.Binary.Get
import Control.Applicative

import Language.ZMachine.Types


data Alphabet = Alpha0 | Alpha1 | Alpha2

-- | Parse a ByteString to a stream of ZChars
--   If there are an odd number of bytes in the bytestring the last byte will be discarded.
--   Reads the ByteString until a stop-bit occurs or until the end.

zstrToZchars :: ZString -> ZChars
zstrToZchars (ZString bs) = Data.Binary.decode (BL.fromStrict bs)

zcharsToZstr :: ZChars -> ZString
zcharsToZstr zchars = ZString $ BL.toStrict $ Data.Binary.encode zchars


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
