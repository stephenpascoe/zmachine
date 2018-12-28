import Test.Hspec
import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Instances

import Data.Bits
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B

import Language.ZMachine.Types
import Language.ZMachine.ZSCII.ZChars

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

  describe "ZString" $ do
    it "Any bytestring will decode to something" $
      property $ \x -> let (ZChars zchars) = zstrToZchars (ZString x)
                       in
                         -- Ignores any trailing odd bytes
                         case B.length x of
                           0 -> zchars == []
                           1 -> zchars == []
                           _ -> length zchars > 0

    it "Any bytestring will roundtrip" $
      property $ \x -> let zstr = ZString $ B.pack x
                           zchars = zstrToZchars zstr
                           zstr' = zcharsToZstr zchars
                       in
                         zstr == zstr'

    -- TODO : test stop-bit logic

-- Quickcheck instances

instance Arbitrary ZChars where
  arbitrary = do
    len <- arbitrary
    ZChars <$> vector len
