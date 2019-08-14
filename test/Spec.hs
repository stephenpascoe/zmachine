{-# LANGUAGE OverloadedStrings #-}

import RIO

import Test.Hspec
import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Instances

import Data.Bits
import Data.Word
import RIO.ByteString as B
import RIO.List as L

import Language.ZMachine.Types
import Language.ZMachine.ZSCII.ZChars
import Language.ZMachine.ZSCII.Parsec


main :: IO ()
main = hspec $ do
  describe "ZChars" $ do
    it "any word decodes to 3 zchars in range 0-31" $
      property $ \x -> let (z1, z2, z3) = unpackZchars x
                           isValidZchar x = x >= 0 && x < 32
                       in
                         isValidZchar z1 && isValidZchar z2 && isValidZchar z3
    it "any word round-trips to itself if the stop bit is not set" $
      property $ \x -> let x' = x .&. 0x7fff
                       in
                         (packZchars . unpackZchars) x' == x'

    it "A single ZChar will encode to 2 bytes" $
      property $ \zchar -> let ZString bs = zcharsToZstr [zchar]
                           in
                             B.length bs == 2

    it "Any non-empty ZChars encode to a bytestring ending with a stop bit" $
      forAll genZChars $ \x -> L.length x > 0 ==> let ZString bs = zcharsToZstr x
                                                      blen = B.length bs
                                                      b1 = B.index bs (blen - 2)
                                                      b2 = B.index bs (blen - 1)

                                                      w :: Word16
                                                      w = (fromIntegral b1) `shift` 8 .|. (fromIntegral b2)
                                                  in
                                                    hasStopBit w

    it "Any sequence of zchars will roundtrip through ZString, possibly with ending padding" $
      forAll genZChars $ \x -> let z = zcharsToZstr x
                                   x' = zstrToZchars z
                                   paddingChar = 5
                                   removePadding zchars =
                                     L.reverse $ L.dropWhile ((==) paddingChar) $ L.reverse zchars
                               in
                                 removePadding x == removePadding x'

  describe "ZString" $ do
    it "Any even bytestring will decode to something" $
      property $ \x -> let zchars = zstrToZchars (ZString x)
                       in
                         -- Ignores any trailing odd bytes
                         case B.length x of
                           0 -> zchars == []
                           1 -> zchars == []
                           _ -> L.length zchars > 0
    it "A padding word is ignored" $ do
      decodeZString 1 Nothing (zcharsToZstr [5, 5, 5]) `shouldBe` (Right $ ZsciiString "")


    -- TODO : test stop-bit logic


-- Quickcheck generators
genZChars :: Gen [ZChar]
genZChars = do len <- arbitrary
               vectorOf len (elements [0..31])
