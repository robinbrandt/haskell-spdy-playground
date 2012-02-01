module Network.SPDY
    ( parse
    , serialize
    , Flag(..)
    , inflateNvHeaders
    , ControlFrameHeader(..)
    , NvHeaders
    , StatusCode (..)
    , Frame(..) )
    where

import Prelude hiding (length)
import Control.Monad (foldM, replicateM)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BS8
import Data.Word as W
import Data.Set as Set hiding (map, filter)
import Data.Binary as Bin
import Data.Binary.Strict.BitGet as BG
import Data.Binary.BitPut as BP
import Data.Binary.Strict.Get as G
import Data.Bits
import Data.Tuple
import qualified Data.Map as Map
import qualified Codec.Zlib as Zlib

type StreamID = Word32

data Flag = FLAG_FIN
	| FLAG_UNIDIRECTIONAL
	| FLAG_SETTINGS_CLEAR_SETTINGS
	deriving (Show, Ord, Eq)

type Flags = Set Flag

type NvHeaders = Map.Map String String

data StatusCode = ProtocolError
	| InvalidStream
	| RefusedStream
	| UnsupportedVersion
	| Cancel
	| FlowControlError
	| StreamInUse
	| StreamAlreadyClosed
	deriving (Show, Eq)

data SettingsEntryId = UploadBandwidth
		      | DownloadBandwidth
		      | RoundTripTime
		      | MaxConcurrentStreams
		      | CurrentCwnd 
		      deriving (Show, Eq)

data SettingsEntryFlag = SettingsPersistValue
		       | SettingsPersisted
		      deriving (Show, Ord, Eq)

type SettingsEntryValue = Word32

type SettingsEntries = [(SettingsEntryId, Set SettingsEntryFlag, SettingsEntryValue)]

data ControlFrameHeader = ControlFrameHeader {
			  version:: Word16
			, flags:: Flags
			, length:: Word32 }
			deriving (Show, Eq)

data ControlFrameType =  SynStreamType
		| SynReplyType
		| RstStreamType
		| SettingsType
		| NoopType
		| PingType
		| GoAwayType
		| HeadersType
		| WindowUpdateType

type ParseZlib a = Zlib.Inflate -> IO a

data Frame = SynStream { 
	      header:: ControlFrameHeader
	    , priority:: Word8
	    , streamId:: StreamID
	    , associatedTo:: Maybe StreamID
	    , headers:: BS.ByteString }
	| SynReply { 
	      header:: ControlFrameHeader
	    , streamId:: StreamID
	    , headers:: BS.ByteString }
	| RstStream {
	      header:: ControlFrameHeader
	    , streamId:: StreamID
	    , status:: StatusCode}
	| Settings {
	      header:: ControlFrameHeader
	    , entries:: SettingsEntries }
	| Noop {
	      header:: ControlFrameHeader }
	| Ping {
	      header:: ControlFrameHeader
	    , id:: Word32 }
	| GoAway {
	      header:: ControlFrameHeader
	    , lastGoodStreamId:: StreamID }	
	| Headers {
	      header:: ControlFrameHeader
	    , streamId:: StreamID
	    , headers:: BS.ByteString }
	| WindowUpdate {
	      header:: ControlFrameHeader
	    , streamId:: StreamID
	    , deltaWindowSize:: Word32 }
	| Data {
	      streamId:: StreamID
	    , dataFlags:: Flags
	    , payload:: BS.ByteString }
	deriving (Show, Eq)
	      

parseFrame :: BG.BitGet Frame
parseFrame = do
    isControl <- BG.getBit
    if isControl 
	then parseControl
	else parseData

parseControl :: BG.BitGet Frame
parseControl = do
	version <- BG.getAsWord16 15
	frameType <- parseControlFrameType
	flags <- parseFlags frameType
	payloadLength <- BG.getAsWord32 24

	let header = ControlFrameHeader version flags payloadLength
	    in parseControlPayload frameType header

parseControlPayload :: ControlFrameType -> ControlFrameHeader -> BG.BitGet Frame
parseControlPayload SynStreamType header = do
	_ <- BG.getBit
	streamId <- BG.getAsWord32 31
	_ <- BG.getBit
	associatedStreamId <- BG.getAsWord32 31
	priority <- parsePriority
	BG.skip 14
	nvHeaders <- parseByteString $ (length header - 10)
	let associated = if associatedStreamId == 0
			 then Nothing
			 else return associatedStreamId
	    in return $ SynStream header priority streamId associated nvHeaders

parseControlPayload SynReplyType header = do
	_ <- BG.getBit
	streamId <- BG.getAsWord32 31
	BG.skip 16
	nvHeaders <- parseByteString $ (length header - 10)
	return $ SynReply header streamId nvHeaders

parseControlPayload RstStreamType header = do
	_ <- BG.getBit
	streamId <- BG.getAsWord32 31
	statusCode <- parseStatusCode
	return $ RstStream header streamId statusCode

parseControlPayload SettingsType header = do
	settingsEntries <- parseSettingsEntries
	return $ Settings header settingsEntries

parseControlPayload PingType header = do
	id <- BG.getWord32be
	return $ Ping header id

parseControlPayload NoopType header = do
	return $ Noop header

parseControlPayload GoAwayType header = do
	BG.skip 1
	lastStreamId <- BG.getAsWord32 31
	return $ GoAway header lastStreamId 

parseControlPayload HeadersType header = do
	BG.skip 1
	streamId <- BG.getAsWord32 31
	BG.skip 16
	nvHeaders <- parseByteString $ (length header - 10)
	return $ Headers header streamId nvHeaders

parseControlPayload WindowUpdateType header = do
	BG.skip 1
	streamId <- BG.getAsWord32 31
	BG.skip 1
	deltaWindowSize <- BG.getAsWord32 31
	return $ WindowUpdate header streamId deltaWindowSize

parseStatusCode :: BG.BitGet StatusCode
parseStatusCode = do
    val <- BG.getWord32be
    return $ case val of
	1 -> ProtocolError
	2 -> InvalidStream
	3 -> RefusedStream
	4 -> UnsupportedVersion
	5 -> Cancel
	6 -> FlowControlError
	7 -> StreamInUse
	8 -> StreamAlreadyClosed

parseSettingsEntries :: BG.BitGet SettingsEntries
parseSettingsEntries = do
    cntEntries <- BG.getWord32be
    values <- sequence $ replicate (fromIntegral cntEntries) parseOneEntry 
    return values
 
    where parseOneEntry = do
		flags <- parseBitSet [(SettingsPersistValue, 1)
				     ,(SettingsPersisted, 2)]
		id <- BG.getAsWord32 24
		val <- BG.getWord32be
		return (UploadBandwidth, flags, val)
    

parseByteString :: Word32 -> BG.BitGet BS.ByteString
parseByteString len = do
    bytes <- BG.getLeftByteString $ (fromIntegral len) * 8
    return bytes

inflateNvHeaders :: Zlib.Inflate -> BS.ByteString -> IO NvHeaders
inflateNvHeaders infl bs = do
    inflate <-  go' infl Prelude.id bs
    final <- Zlib.finishInflate infl
    let inflated = BS.concat $ inflate [final] in
	case runBitGet inflated parseNvHeader of
	    Left err -> fail err
	    Right headers -> return headers
    where
	go' infl front bs = Zlib.withInflateInput infl bs $ go front
	go front x = do
	    y <- x
	    case y of
		Nothing -> return front
		Just z -> go (front . (:) z) x 

parseNvHeader :: BG.BitGet NvHeaders
parseNvHeader = do
    numPairs <- BG.getWord16be
    nvPairs <- replicateM (fromIntegral numPairs) parsePairs
    return $ Map.fromList nvPairs
    where
	parsePairs = do
	    nameLen <- BG.getWord16be
	    name <- getString $ fromIntegral nameLen
	    valueLen <- BG.getWord16be
	    value <- getString $ fromIntegral valueLen
	    return (name, value)
	getString len = do
	    bs <- getLeftByteString (len * 8)
	    str <- return $ BS8.unpack bs
	    return str

parsePriority :: BG.BitGet Word8
parsePriority = BG.getAsWord8 2 

parseData :: BG.BitGet Frame
parseData = do
    streamId <- BG.getAsWord32 31
    flags <- parseBitSet [(FLAG_FIN, 1)]
    len <- BG.getAsWord32 24
    bs <- parseByteString len
    return $ Data streamId flags bs

parseControlFrameType :: BG.BitGet ControlFrameType
parseControlFrameType = do
    ftype <- BG.getWord16be
    return $ case ftype of
	1 -> SynStreamType
	2 -> SynReplyType
	3 -> RstStreamType
	4 -> SettingsType
	5 -> NoopType
	6 -> PingType
	7 -> GoAwayType
	8 -> HeadersType
	9 -> WindowUpdateType

parseFlags :: ControlFrameType -> BG.BitGet Flags
parseFlags SynStreamType = parseBitSet [(FLAG_FIN, 1)
				      , (FLAG_UNIDIRECTIONAL, 2)]
parseFlags SynReplyType = parseBitSet  [(FLAG_FIN, 1)]
parseFlags HeadersType = parseBitSet [(FLAG_FIN, 1)]
parseFlags SettingsType = parseBitSet [(FLAG_SETTINGS_CLEAR_SETTINGS, 1)]

parseFlags _ = parseBitSet []

parseBitSet :: Ord a => [(a, Word8)] -> BG.BitGet (Set a)
parseBitSet lst = do
    flags <- BG.getWord8
    let onlySetBits = \(flag, bitValue) -> flags .&. bitValue == bitValue
	in return $ Set.fromList $ map fst $ filter onlySetBits lst

parse :: BS.ByteString -> Maybe Frame
parse bs = case BG.runBitGet bs parseFrame of
	    Left err -> Nothing
	    Right frame -> Just frame 

-- | Serialization 
--
--

serialize :: Frame -> BS.ByteString
serialize fr = BS.concat $ BL.toChunks lbs
    where
	lbs = BP.runBitPut $ putFrame fr

putFrame :: Frame -> BP.BitPut
putFrame (Data streamId flags payload) = do
    BP.putBit False
    putStreamId streamId
    putFlags flags
    BP.putNBits 24 (BS.length payload)
    putPayload payload

putFrame (SynStream controlHeaders prio streamId associatedStreamID headers) = do
    putControlHeader [ (FLAG_FIN, 1)
		     , (FLAG_UNIDIRECTIONAL, 2)]
		     1
		     controlHeaders
    BP.putBit False
    putStreamId streamId
    BP.putBit False
    putOptionalStreamId associatedStreamID
    putPrio prio
    BP.putNBits 14 $ (0 :: Word8)
    BP.putByteString headers 

putFrame _ = fail "not implemented yet"

putOptionalStreamId optId =
    case optId of
	Just s -> BP.putNBits 31 (s :: Word32)
	Nothing -> BP.putNBits 31 (0 :: Word8)

putPrio :: Word8 -> BP.BitPut
putPrio = putNBits 2

putControlHeader flagList frameType (ControlFrameHeader version flags length) = do
    BP.putBit True
    BP.putNBits 15 (version :: Word16)
    BP.putNBits 16 (frameType :: Word16)
    putBitSet flagList flags
    BP.putNBits 24 length


putStreamId :: StreamID -> BP.BitPut
putStreamId id = BP.putNBits 31 id

putFlags :: Flags -> BP.BitPut
putFlags = putBitSet [(FLAG_FIN, 1)]

putBitSet :: Ord a => [(a, Word8)] -> Set a -> BP.BitPut
putBitSet lst flags = do
    BP.putNBits 8 flagsAsVal
    where
	flagsAsVal = Set.fold (setBits' lst) 0 flags

setBits' :: Ord a => [(a, Word8)] -> a -> Word8 -> Word8
setBits' lst val acc = acc .|. (bitValue val)
    where
	bitValue = \val -> case Map.lookup val lookupTable of
				Nothing -> 0
				Just val -> val
	lookupTable = Map.fromList lst
	

putPayload :: BS.ByteString -> BP.BitPut
putPayload bs =
    BP.putByteString bs

