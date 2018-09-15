import Test.Hspec
import Test.QuickCheck hiding ((.&.))

import Data.Bits

import Lib.ZSCII

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
