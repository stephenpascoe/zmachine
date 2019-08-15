module Language.ZMachine.ZSCII
  ( decodeZString
  , zseqToText
  , ZsciiException(..)
  ) where

import RIO

import qualified Data.Text.Encoding as TE
import qualified RIO.Text as T

import Language.ZMachine.ZSCII.Parsec
import Language.ZMachine.Types

zseqToText :: ZsciiString -> T.Text
zseqToText (ZsciiString bstr) = TE.decodeLatin1 bstr
