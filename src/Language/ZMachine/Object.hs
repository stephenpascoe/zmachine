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
import qualified Language.ZMachine.Abbreviations       as A
import           Language.ZMachine.App          ( App )
import Language.ZMachine.Abbreviations ( getAbbreviationTable ) 

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

type PropertyDefaults = V.Vector Word32

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
    (fmap f $ zip [(1 :: Int) ..] (V.toList objs))
    where f (i, obj) = display i <> ". " <> display obj <> "\n"

-- Slightly different representation of Object, useful during decoding
data ObjectRec = ObjectRec
  { attributes'   :: B.ByteString
  , parentId'     :: Word16
  , siblingId'    :: Word16
  , childId'      :: Word16
  , propertyAddr' :: M.ByteAddress
  }


readObjects :: (M.HasMemory env, A.HasAbbreviations env) => RIO env ObjectTable
readObjects = do
  header <- M.getHeader
  let offset = M.objectTable header
      propDefaultsLen =
        if M.zVersionToInt (M.zVersion header) < 4 then 31 else 63
  propDefaults <- readPropDefaults offset propDefaultsLen
  objRecs <- readObjectRecs (offset + (fromIntegral propDefaultsLen * 4) + 1)
  properties <- traverse f objRecs
  return $ ObjectTable propDefaults (V.fromList $ zipWith g objRecs properties)
 where
  f objRec = readPropertyTable (propertyAddr' objRec)
  g obj (desc, props) = Object { attributes  = attributes' obj
                               , description = desc
                               , parentId    = fromIntegral $ parentId' obj
                               , siblingId   = fromIntegral $ siblingId' obj
                               , childId     = fromIntegral $ childId' obj
                               , properties  = props
                               }

-- Read the property defaults table
readPropDefaults
  :: M.HasMemory env => M.ByteAddress -> Int -> RIO env PropertyDefaults
readPropDefaults offset recs = do
  stream <- M.streamBytes (fromIntegral offset)
  return $ runGet f stream
  where f = sequenceA $ V.replicate recs getWord32be


-- Read ObjectRec remembering the number of bytes read
readObjectRec
  :: M.HasMemory env => M.ByteAddress -> RIO env (ObjectRec, M.ByteAddress)
readObjectRec offset = do
  stream <- M.streamBytes (fromIntegral offset)
  header <- M.getHeader
  return $ runGet (f (M.zVersion header)) stream where
  f v = do
    objRec <- decodeObjectRec v
    bytes  <- bytesRead
    return $ (objRec, offset + (fromIntegral bytes))

readObjectRecs
  :: M.HasMemory env
  => M.ByteAddress  -- ^ Start reading at this address
  -> RIO env [ObjectRec]
readObjectRecs offset = f offset Nothing where
  f
    :: M.HasMemory env
    => M.ByteAddress
    -> Maybe M.ByteAddress
    -> RIO env [ObjectRec]
  f offset Nothing = do
    (objRec, offset') <- readObjectRec offset
    let readUntil = propertyAddr' objRec
    rest <- f offset' (Just readUntil)
    return $ (objRec : rest)
  f offset (Just readUntil) = if offset >= readUntil
    then return []
    else do
      (objRec, offset') <- readObjectRec offset
      let readUntil' = max readUntil (propertyAddr' objRec)
      rest <- f offset' (Just readUntil')
      return $ (objRec : rest)


readPropertyTable
  :: (M.HasMemory env, A.HasAbbreviations env) => M.ByteAddress -> RIO env (Z.ZsciiString, [Property])
readPropertyTable offset = do
  header <- M.getHeader
  stream <- M.streamBytes (fromIntegral offset)
  -- TODO : Parse AbbreviationTable, Strore in env
  aTable <- getAbbreviationTable
  return $ runGet (decodePropertyTable (M.zVersion header) (Just aTable)) stream


decodePropertyBlock :: M.ZVersion -> Get (Maybe Property)
decodePropertyBlock version
  | (M.zVersionToInt version) < 4 = do
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


decodePropertyHeader
  :: M.ZVersion -> Maybe Z.AbbreviationTable -> Get Z.ZsciiString
decodePropertyHeader version aTable = do
  size  <- getWord8
  zdata <- getByteString (fromIntegral size)
  -- TODO : Abbreviation table support
  return $ Z.decodeZString version aTable (BL.fromStrict zdata)

decodePropertyTable
  :: M.ZVersion -> Maybe Z.AbbreviationTable -> Get (Z.ZsciiString, [Property])
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


decodeObjectRec :: M.ZVersion -> Get ObjectRec
decodeObjectRec version
  | (M.zVersionToInt version) < 4 = do
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
