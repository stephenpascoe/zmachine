module Language.ZMachine.Object
    ( HasObjects(..)
    , ObjectTable
    , Object(..)
    , Property(..)
    )
where

import           RIO
import qualified RIO.Text                      as T
import qualified RIO.ByteString                as B
import qualified RIO.ByteString.Lazy           as BL
import qualified RIO.Vector.Boxed              as V
import           Hexdump                        ( simpleHex )

import qualified Language.ZMachine.Memory      as M
import qualified Language.ZMachine.ZSCII       as Z
import qualified Language.ZMachine.Abbreviations
                                               as A
import           Language.ZMachine.App          ( App )
import           Language.ZMachine.Abbreviations
                                                ( getAbbreviationTable )

import           Data.Binary.Get
import           Data.Bits

-- TODO : Default properties table
-- TODO : buildTree

class HasObjects env where
  getObjects :: M.HasMemory env => RIO env ObjectTable

instance HasObjects App where
    getObjects = readObjects


-- | Properties are unstructured binary data
data Property = Property Int8 ByteString
    deriving Show

data Object = Object
    { attributes  :: B.ByteString
    , description :: Z.ZsciiString
    , parentId    :: Integer
    , siblingId   :: Integer
    , childId     :: Integer
    , properties  :: [Property]
    }
    deriving Show

type PropertyDefaults = V.Vector Word16

data ObjectTable = ObjectTable PropertyDefaults (V.Vector Object)

instance Display Property where
    display (Property n propData) =
        "[" <> display n <> "] " <> display (T.pack . simpleHex $ propData)

instance Display Object where
    display obj =
        "Attributes: "
            <> display (T.pack . simpleHex . attributes $ obj)
            <> "\n"
            <> "Parent: "
            <> display (parentId obj)
            <> "  "
            <> "Sibling: "
            <> display (siblingId obj)
            <> "  "
            <> "Child: "
            <> display (childId obj)
            <> "\n"
            <> "Description: \""
            <> (display . Z.zseqToText $ description obj)
            <> "\"\n"
            <> "Properties:\n"
            <> props
      where
        props = mconcat (fmap f (properties obj))
        f prop = "  " <> display prop <> "\n"

-- TODO : Display defaults?
instance Display ObjectTable where
    display (ObjectTable propDefaults objs) = mconcat
        (f <$> zip [(1 :: Int) ..] (V.toList objs))
        where f (i, obj) = display i <> ". " <> display obj <> "\n"

-- Slightly different representation of Object, useful during decoding
data ObjectRec = ObjectRec
    { attributes'   :: B.ByteString
    , parentId'     :: Word16
    , siblingId'    :: Word16
    , childId'      :: Word16
    , propertyAddr' :: M.ByteAddress
    }


{-
    Determining the number of objects is difficult.  There is no count but
    we can assume the last object is immediately followed by the first property list

-}
readObjects :: (M.HasMemory env, A.HasAbbreviations env) => RIO env ObjectTable
readObjects = do
    header <- M.getHeader
    let offset = M.objectTable header
    stream <- M.streamBytes (fromIntegral offset)
    let (propDefaults, objRecs) = runGet (decodeObjectTable (M.zVersion header) offset) stream
    properties <- traverse f objRecs
    return $ ObjectTable propDefaults
                         (V.fromList $ zipWith g objRecs properties)
  where
    f objRec = readPropertyTable (propertyAddr' objRec)
    g obj (desc, props) = Object { attributes  = attributes' obj
                                 , description = desc
                                 , parentId    = fromIntegral $ parentId' obj
                                 , siblingId   = fromIntegral $ siblingId' obj
                                 , childId     = fromIntegral $ childId' obj
                                 , properties  = props
                                 }




decodeObjectTable :: M.ZVersion -> M.ByteAddress -> Get (PropertyDefaults, [ObjectRec])
decodeObjectTable version offset = do
  propDefaults <- decodePropertyDefaults version
  objRecs <- decodeObjectRecs version offset Nothing 
  return (propDefaults, objRecs)

decodeObjectRec :: M.ZVersion -> Get ObjectRec
decodeObjectRec version
    | M.zVersionToInt version < 4 = do
        attributes   <- getByteString 4
        parentId'    <- getWord8
        siblingId'   <- getWord8
        childId'     <- getWord8
        propertyAddr <- getWord16be
        return ObjectRec { attributes'   = attributes
                         , parentId'     = fromIntegral parentId'
                         , siblingId'    = fromIntegral siblingId'
                         , childId'      = fromIntegral childId'
                         , propertyAddr' = propertyAddr
                         }
    | otherwise = do
        attributes'   <- getByteString 6
        parentId'     <- getWord16be
        siblingId'    <- getWord16be
        childId'      <- getWord16be
        propertyAddr' <- getWord16be
        return ObjectRec { .. }




decodePropertyDefaults :: M.ZVersion -> Get PropertyDefaults
decodePropertyDefaults v | M.zVersionToInt v < 4 =
  V.replicateM 31 getWord16be
decodePropertyDefaults _ =
  V.replicateM 63 getWord16be

decodeObjectRecs
  :: M.ZVersion -- ^ Interpreter version
  -> M.ByteAddress -- ^ Address of start of objects
  -> Maybe M.ByteAddress -- ^ Maxumum end of objects
  -> Get [ObjectRec] -- ^ All Object records
decodeObjectRecs v start mEnd =
  do pos <- bytesRead
     orec <- decodeObjectRec v
     let end = case mEnd of
                  Just end' -> min end' (propertyAddr' orec)
                  Nothing -> propertyAddr' orec
     if fromIntegral pos + start >= end
       then return []
       else do rest <- decodeObjectRecs v start (Just end)
               return (orec : rest)


readPropertyTable
    :: (M.HasMemory env, A.HasAbbreviations env)
    => M.ByteAddress
    -> RIO env (Z.ZsciiString, [Property])
readPropertyTable offset = do
    header <- M.getHeader
    stream <- M.streamBytes (fromIntegral offset)
    aTable <- getAbbreviationTable
    return $ runGet (decodePropertyTable (M.zVersion header) aTable)
                    stream


decodePropertyTable
    :: M.ZVersion
    -> Z.AbbreviationTable
    -> Get (Z.ZsciiString, [Property])
decodePropertyTable version aTable = do
    description <- decodePropertyHeader version aTable
    props       <- f version
    return (description, props)
  where
    f v = do
        mProp <- decodePropertyBlock version
        case mProp of
            Nothing   -> return []
            Just prop -> do
                props <- f v
                return (prop : props)



decodePropertyHeader
    :: M.ZVersion -> Z.AbbreviationTable -> Get Z.ZsciiString
decodePropertyHeader version aTable = do
    size  <- getWord8
    zdata <- getByteString (fromIntegral size * 2)
    case Z.decodeZString version (Just aTable) (BL.fromStrict zdata) of
      Left e -> fail $ T.unpack e
      Right zscii -> return zscii


decodePropertyBlock :: M.ZVersion -> Get (Maybe Property)
decodePropertyBlock version
    | M.zVersionToInt version < 4 = do
        sizeByte <- getWord8
        if sizeByte == 0
            then return Nothing
            else do
                let size    = fromIntegral $ (sizeByte + 1) `quot` 32
                    propNum = fromIntegral $ (sizeByte + 1) `mod` 32
                pdata <- getByteString size
                return $ Just (Property propNum pdata)
    | otherwise = do
        sizeByte1 <- getWord8
        -- Next byte is conditional on bit 7
        if sizeByte1 == 0
            then return Nothing
            else if testBit sizeByte1 7
                then do
                    sizeByte2 <- getWord8
                    let propNum = fromIntegral $ sizeByte1 .&. 0x1f
                        size    = sizeByte2 .&. 0x1f
                    pdata <- getByteString (fromIntegral size)
                    return $ Just (Property propNum pdata)
                else do
                    let propNum = fromIntegral $ sizeByte1 .&. 0x1f
                        size    = if testBit sizeByte1 6 then 2 else 1
                    pdata <- getByteString size
                    return $ Just (Property propNum pdata)


