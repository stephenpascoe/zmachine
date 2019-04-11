{-# LANGUAGE OverloadedStrings #-}

module Language.ZMachine.ZSCII.Parsec
 ( decodeZString
 ) where

import qualified Data.ByteString as B
import Data.Word
import Data.Vector ((!?))
import Text.Parsec.Prim
import Text.Parsec.Combinator
import Data.Maybe
import Data.Foldable

import Language.ZMachine.Types
import Language.ZMachine.ZSCII.ZChars

data Alphabet = Alpha0 | Alpha1 | Alpha2

type ZsciiParsec = Parsec [ZChar] Alphabet



getAlphabetTable :: Version -> Alphabet -> B.ByteString
getAlphabetTable 1 Alpha0 = "abcdefghijklmnopqrstuvwxyz"
getAlphabetTable 1 Alpha1 = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
getAlphabetTable 1 Alpha2 = "^0123456789.,!?_#'\"/\\<-:()"
getAlphabetTable _ Alpha0 = "abcdefghijklmnopqrstuvwxyz"
getAlphabetTable _ Alpha1 = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
getAlphabetTable _ Alpha2 = "^\n0123456789.,!?_#'\"/\\-:()"


-- TODO : Put in Either monad for error reporting

decodeZString :: Version                 -- ^ ZMachine version
              -> Maybe AbbreviationTable -- ^ Abbreviations, if available
              -> ZString                 -- ^ Input ZString
              -> ZsciiString             -- ^ Resulting ZsciiString
decodeZString version aTable zstr = zcharsToZscii version aTable (zstrToZchars zstr)


data Token = ZCharToken Word8 | AbrevToken ZsciiString | EmptyToken

foldTokens :: [Token] -> ZsciiString
foldTokens toks = ZsciiString $ foldl' f "" toks where
  f :: B.ByteString -> Token -> B.ByteString
  f acc EmptyToken = acc
  f acc (AbrevToken (ZsciiString abrev)) = B.append acc abrev
  f acc (ZCharToken zchar) = B.snoc acc zchar

zcharsToZscii :: Version -> Maybe AbbreviationTable -> [ZChar] -> ZsciiString
zcharsToZscii version aTable zchars =
  let alphabetTable = getAlphabetTable version
      init = Alpha0

      pureChar :: Char -> ZsciiParsec Word8
      pureChar = pure . fromIntegral . fromEnum

      -- TODO : deal with Token
      parseZstring :: ZsciiParsec ZsciiString
      parseZstring = foldTokens <$> many element

      element = normal <|> special

      -- Basic token parsers
      satisfy :: (ZChar -> Bool) -> ZsciiParsec ZChar
      satisfy f = tokenPrim (\c -> show [c])
                            -- position calculation is disabled, just return the current position
                            (\pos _c _cs -> pos)
                            (\c -> if f c then Just c else Nothing)

      specialChar = satisfy (\z -> z <= 6)
      normalChar = satisfy (\z -> z > 6 && z < 32)

      -- Many combinators return Maybe a so we can change state without emmiting chars and
      -- allowing us to handle padding at the end of a string.

      -- Alphabet shifting
      shiftUp = do a <- getState
                   putState $ case a of
                                Alpha0 -> Alpha1
                                Alpha1 -> Alpha2
                                Alpha2 -> Alpha0

      shiftDown = do a <- getState
                     putState $ case a of
                                  Alpha0 -> Alpha2
                                  Alpha1 -> Alpha0
                                  Alpha2 -> Alpha1

      shiftUpOnce = do shiftUp
                       e <- element <|> pure EmptyToken
                       shiftDown
                       return e

      shiftDownOnce = do shiftDown
                         e <- element <|> pure EmptyToken
                         shiftUp
                         return e

      -- Special character parser
      special = if version < 3 then specialV12 else specialV3

      specialV12 = do z <- specialChar
                      case z of
                        0 -> ZCharToken <$> pureChar ' '
                        1 -> if version == 1 then ZCharToken <$> pureChar '\n'
                             else AbrevToken <$> abbrev 1
                        2 -> shiftUpOnce
                        3 -> shiftDownOnce
                        4 -> shiftDown *> pure EmptyToken
                        5 -> shiftUp *> pure EmptyToken
                        6 -> ZCharToken <$> pureChar '@' -- error "char 6" -- handle char 6 in A2
                        _ -> error "Not a special character"

      specialV3 = do z <- specialChar
                     case z of
                       0 -> ZCharToken <$> pureChar ' '
                       1 -> AbrevToken <$> abbrev 1
                       2 -> AbrevToken <$> abbrev 2
                       3 -> AbrevToken <$> abbrev 3
                       4 -> shiftUpOnce
                       5 -> shiftDownOnce
                       6 -> ZCharToken <$> pureChar '@' -- error "char 6" -- handle char 6 in A2
                       _ -> error "Not a special character"

      -- Normal character parser
      normal = do z <- normalChar
                  alphabet <- getState
                  return $ ZCharToken (B.index (getAlphabetTable version alphabet) (fromIntegral (z - 6)))

      -- Abbreviation parser
      -- TODO : Need to return ByteString
      abbrev :: Integer -> ZsciiParsec ZsciiString
      abbrev _ = pure $ ZsciiString "<ABREV>"
{-
      abbrev x = do z <- anyToken
                    return $ getAbbreviation aTable x z
-}
  in
    case runParser parseZstring init "ZString decoder" zchars of
      Left e -> error $ show e
      Right zscii -> zscii



-- TODO : Replace error with exception monad
getAbbreviation :: Maybe AbbreviationTable -> Int -> Word8 -> ZsciiString
getAbbreviation Nothing _ _ = error "No abbreviations available"
getAbbreviation (Just t) a b = case t !? (fromIntegral (a * 32) + fromIntegral b) of
                                 Nothing -> error "Abbreviation index out of range"
                                 Just x -> x
