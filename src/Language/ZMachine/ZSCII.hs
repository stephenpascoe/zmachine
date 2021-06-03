module Language.ZMachine.ZSCII
    ( decodeZString
    , zseqToText
    , ZsciiString(..)
    , ZString(..)
    , AbbreviationTable
    )
where

import           RIO
import qualified Data.Text.Encoding            as TE
import qualified RIO.Text                      as T

import           Language.ZMachine.ZSCII.Parsec
import           Language.ZMachine.ZSCII.ZChars

zseqToText :: ZsciiString -> T.Text
zseqToText (ZsciiString bstr) = TE.decodeLatin1 bstr
