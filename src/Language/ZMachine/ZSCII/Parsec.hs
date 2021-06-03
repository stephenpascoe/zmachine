module Language.ZMachine.ZSCII.Parsec
    ( decodeZString
    , ZsciiException(..)
    )
where

import           RIO                     hiding ( many
                                                , try
                                                , (<|>)
                                                )

import qualified RIO.ByteString                as B
import qualified RIO.Text                      as T
import           RIO.Vector.Boxed               ( (!?) )
import           Text.Parsec.Prim
import           Text.Ascii
import           Data.Bits

import           Language.ZMachine.ZSCII.ZChars
import qualified Language.ZMachine.Memory      as M


data ZsciiException = ZsciiException T.Text
    deriving (Show, Typeable)
instance Exception ZsciiException

data Alphabet = Alpha0 | Alpha1 | Alpha2 deriving Eq

type ZsciiParsec = Parsec [ZChar] Alphabet



getAlphabetTable :: M.ZVersion -> Alphabet -> B.ByteString
getAlphabetTable M.ZVer1 Alpha0 = "abcdefghijklmnopqrstuvwxyz"
getAlphabetTable M.ZVer1 Alpha1 = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
getAlphabetTable M.ZVer1 Alpha2 = "^0123456789.,!?_#'\"/\\<-:()"
getAlphabetTable _       Alpha0 = "abcdefghijklmnopqrstuvwxyz"
getAlphabetTable _       Alpha1 = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
getAlphabetTable _       Alpha2 = "^\n0123456789.,!?_#'\"/\\-:()"


-- TODO :: Convert to HasAbbreviations 
decodeZString
    :: M.ZVersion                -- ^ ZMachine version
    -> Maybe AbbreviationTable   -- ^ Abbreviations, if available
    -> ZString                   -- ^ Input ZString
    -> ZsciiString               -- ^ Resulting ZsciiString or error
decodeZString version aTable zstr =
    zcharsToZscii version aTable (zstrToZchars zstr)

-- | Token is used inside the Zstring parser to handle shift events and abreviations
data Token = ZCharToken Word8 | AbrevToken ZsciiString | EmptyToken

foldTokens :: [Token] -> ZsciiString
foldTokens toks = ZsciiString $ foldl' f "" toks  where
    f :: B.ByteString -> Token -> B.ByteString
    f acc EmptyToken                       = acc
    f acc (AbrevToken (ZsciiString abrev)) = B.append acc abrev
    f acc (ZCharToken zchar              ) = B.snoc acc zchar

zcharsToZscii :: M.ZVersion -> Maybe AbbreviationTable -> [ZChar] -> ZsciiString
zcharsToZscii version aTable zchars =
    let
        parseZstring :: ZsciiParsec ZsciiString
        parseZstring = foldTokens <$> many element

        element      = normal <|> special

        -- Basic token parsers
        satisfy :: (ZChar -> Bool) -> ZsciiParsec ZChar
        satisfy f = tokenPrim (\c -> show [c])
                              -- position calculation is disabled, just return the current position
                              (\pos _c _cs -> pos)
                              (\c -> if f c then Just c else Nothing)

        specialChar = satisfy (\z -> z < 6)
        normalChar  = satisfy (\z -> z >= 6 && z < 32)
        anyChar     = satisfy (const True)

        -- Alphabet shifting
        shiftUp     = do
            a <- getState
            putState $ case a of
                Alpha0 -> Alpha1
                Alpha1 -> Alpha2
                Alpha2 -> Alpha0

        shiftDown = do
            a <- getState
            putState $ case a of
                Alpha0 -> Alpha2
                Alpha1 -> Alpha0
                Alpha2 -> Alpha1

        shiftUpOnce = do
            shiftUp
            e <- element <|> pure EmptyToken
            shiftDown
            return e

        shiftDownOnce = do
            shiftDown
            e <- element <|> pure EmptyToken
            shiftUp
            return e

        -- Special character parser
        special =
            if (M.zVersionToInt version) < 3 then specialV12 else specialV3

        specialV12 = do
            z <- specialChar
            case z of
                0 -> pure $ ZCharToken (ascii ' ')
                1 -> case version of
                    M.ZVer1 -> pure $ ZCharToken (ascii '\n')
                    _       -> abbrev 1
                2 -> shiftUpOnce
                3 -> shiftDownOnce
                4 -> shiftDown *> pure EmptyToken
                5 -> shiftUp *> pure EmptyToken

                _ -> fail "Not a special character"

        specialV3 = do
            z <- specialChar
            case z of
                0 -> pure $ ZCharToken (ascii ' ')
                1 -> abbrev 1
                2 -> abbrev 2
                3 -> abbrev 3
                4 -> shiftUpOnce
                5 -> shiftDownOnce

                _ -> fail "Not a special character"

        -- Normal character parser

        normal = do
            z        <- normalChar
            alphabet <- getState

            if alphabet == Alpha2 && z == 6
                then asciiChar
                else
                    return
                        $ ZCharToken
                              (B.index (getAlphabetTable version alphabet)
                                       (fromIntegral (z - 6))
                              )

        -- ZChar 6 in A2 parses the next 2 ZChars as an ASCII code
        -- TODO : If we reach the end of input before 2 bytes are read return empty
        --        this is needed to parse the dictionary of LostPig.  we might want to
        --        handle this error elsewhere.
        asciiChar  = try asciiChar' <|> restOfInput

        asciiChar' = do
            zc1 <- anyChar
            zc2 <- anyChar
            return $ ZCharToken $ (zc1 `shift` 5) .|. zc2


        restOfInput = many anyChar *> pure EmptyToken

        -- Abbreviation parser
        abbrev :: ZChar -> ZsciiParsec Token
        abbrev x = do
            z <- anyChar
            return $ getAbbreviation aTable x z
    in
        case runParser parseZstring Alpha0 "ZString decoder" zchars of
            Left  e     -> impureThrow $ ZsciiException (T.pack (show e))
            Right zscii -> zscii


getAbbreviation :: Maybe AbbreviationTable -> ZChar -> ZChar -> Token
getAbbreviation Nothing _ _ =
  -- impureThrow $ ZsciiException "No abbreviations available"
    AbrevToken "-"

getAbbreviation (Just t) a b = case t !? (((fromIntegral a - 1) * 32) + fromIntegral b) of
    Nothing -> impureThrow $ ZsciiException ("Abbreviation index out of range : " <> (T.pack . show $ a) <> " " <> (T.pack . show $ b))
    Just x  -> AbrevToken x
