import Test.Hspec
import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Instances

import Data.Bits
import Data.Binary.Get
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B

import Language.ZMachine.ZSCII

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

    it "Any bytestring will decode to something" $
      property $ \x -> let zchars = runGet decodeZchars (BL.fromStrict x)
                       in
                         -- Ignores any trailing odd bytes
                         case B.length x of
                           0 -> zchars == []
                           1 -> zchars == []
                           _ -> length zchars > 0

    -- TODO : test stop-bit logic
