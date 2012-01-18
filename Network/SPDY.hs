module Network.SPDY
    ( parse

    , ControlFrameHeader(ControlFrameHeader)
    , Frame(Ping) )
    where


import qualified Data.ByteString as BS
import Data.Word as W
import Data.Set as Set hiding (map, filter)
import Data.Binary as Bin
import Data.Binary.Strict.BitGet as BG
import Data.Binary.Strict.Get as G
import Data.Bits
import qualified Data.Map as Map

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
		| PingType
		| GoAwayType
		| HeadersType
		| WindowUpdateType

data Frame = SynStream { 
	      header:: ControlFrameHeader
	    , priority:: Word8
	    , streamId:: StreamID
	    , associatedTo:: Maybe StreamID
	    , headers:: NvHeaders }
	| SynReply { 
	      header:: ControlFrameHeader
	    , streamId:: StreamID
	    , headers:: NvHeaders }
	| RstStream {
	      header:: ControlFrameHeader
	    , streamId:: StreamID
	    , status:: StatusCode}
	| Settings {
	      header:: ControlFrameHeader
	    , entries:: SettingsEntries }
	| Ping {
	      header:: ControlFrameHeader
	    , id:: Word32 }
	| GoAway {
	      header:: ControlFrameHeader
	    , lastGoodStreamId:: StreamID
	    , status:: StatusCode }	
	| Headers {
	      header:: ControlFrameHeader
	    , streamId:: StreamID
	    , headers:: NvHeaders }
	| WindowUpdate {
	      header:: ControlFrameHeader
	    , streamId:: StreamID
	    , deltaWindowSize:: Word32 }
	| Data {
	      streamId:: StreamID
	    , dateFlags:: Flags
	    , payload:: BS.ByteString }
	deriving (Show, Eq)
	      
parseFrame :: G.Get Frame
parseFrame = do
    header <- G.getByteString 16
    
    let r = BG.runBitGet header $ do
	isControl <- BG.getBit
	if isControl 
	    then parseControl
	    else parseData

    case r of
	Left err -> fail err
	Right r -> return r


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
	_ <- BG.getAsWord8 5
	nvHeaders <- parseNvHeaders
	let associated = if associatedStreamId == 0
			 then Nothing
			 else return associatedStreamId
	    in return $ SynStream header priority streamId associated nvHeaders

parseControlPayload SynReplyType header = do
	_ <- BG.getBit
	streamId <- BG.getAsWord32 31
	nvHeaders <- parseNvHeaders
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


parseControlPayload GoAwayType header = do
	BG.skip 1
	lastStreamId <- BG.getAsWord32 31
	statusCode <- parseStatusCode
	return $ GoAway header lastStreamId statusCode

parseControlPayload HeadersType header = do
	BG.skip 1
	lastStreamId <- BG.getAsWord32 31
	nvHeaders <- parseNvHeaders
	return $ Headers header lastStreamId nvHeaders


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
		flags <- parseBitSet 8 [(SettingsPersistValue, 1)
				       ,(SettingsPersisted, 2)]
		id <- BG.getAsWord32 24
		val <- BG.getWord32be
		return (UploadBandwidth, flags, val)
    

-- | Todo: decompression with 
parseNvHeaders :: BG.BitGet NvHeaders
parseNvHeaders = return $ Map.empty
    

parsePriority :: BG.BitGet Word8
parsePriority = BG.getAsWord8 3

parseData :: BG.BitGet Frame
parseData = do
    return $ Data 0 Set.empty BS.empty

parseControlFrameType :: BG.BitGet ControlFrameType
parseControlFrameType = do
    ftype <- BG.getWord16be
    return $ case ftype of
	1 -> SynStreamType
	2 -> SynReplyType
	3 -> RstStreamType
	4 -> SettingsType
	6 -> PingType
	7 -> GoAwayType
	8 -> HeadersType
	9 -> WindowUpdateType

parseFlags :: ControlFrameType -> BG.BitGet Flags
parseFlags SynStreamType = parseBitSet 8 [(FLAG_FIN, 1)
				      , (FLAG_UNIDIRECTIONAL, 2)]
parseFlags SynReplyType = parseBitSet 8  [(FLAG_FIN, 1)]
parseFlags HeadersType = parseBitSet 8 [(FLAG_FIN, 1)]
parseFlags SettingsType = parseBitSet 8 [(FLAG_SETTINGS_CLEAR_SETTINGS, 1)]

parseFlags _ = return Set.empty

parseBitSet :: Ord a => Int -> [(a, Word32)] -> BG.BitGet (Set a)
parseBitSet len lst = do
    flags <- BG.getAsWord32 len
    let onlySetBits = \(flag, bitValue) -> flags .&. bitValue == bitValue
	in return $ Set.fromList $ map fst $ filter onlySetBits lst

parse :: BS.ByteString -> Maybe Frame
parse bs = case G.runGet parseFrame bs of
	    (Left err, _) -> Nothing
	    (Right frame, _) -> Just frame 

