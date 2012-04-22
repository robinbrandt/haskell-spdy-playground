module Network.SPDY
    ( initInflate
    , initDeflate
    , main)
    where

import Data.ByteString as BS hiding (head) 
import Data.ByteString.Char8 as BS8 hiding (head) 
import Data.Set as Set
import Data.Map as Map
import Debug.Trace
import Codec.Zlib (defaultWindowBits)
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import Network.BSD
import System.IO
import qualified Data.ByteString.UTF8 as SU8

import Network.SPDY.Frame as SP
import qualified Codec.Zlib as Zlib

port = "1115"

serveSpdy :: IO ()
serveSpdy = withSocketsDo $
    do
	print $ "Serving spdy on port " ++ port
	addrinfos <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]})) Nothing (Just port)
	let serveraddr = head addrinfos

	sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    
	bindSocket sock (addrAddress serveraddr)
	
	listen sock 5
	
	lock <- newMVar ()

	processRequests lock sock
    where
	processRequests :: MVar () -> Socket -> IO ()
	processRequests lock mastersocket = do
	    (connsock, clientaddr) <- accept mastersocket
	    handle <- socketToHandle connsock ReadWriteMode
	    
	    forkIO $ procMessage lock handle clientaddr
	    processRequests lock mastersocket


data SpdyConnectionState = SpdyConnectionState {
		zlibInflate :: Zlib.Inflate 
	      , zlibDeflate :: Zlib.Deflate }	

data SpdyFrameResponse = SendFrames {
			  frames :: [SP.Frame] }
		       | Ignore
		       | Close

initState :: IO SpdyConnectionState
initState = do
    inflate <- initInflate
    deflate <- initDeflate
    return $ SpdyConnectionState inflate deflate

procMessage lock handle clientaddr = do
    putTraceMsg "A new client connected"
    state <- initState

    let loop = do
	hWaitForInput handle (-1)	
	recvdata <- BS.hGetSome handle 4096
	putTraceMsg ("received " ++ (show $ BS8.length recvdata ) ++ " bytes")
	resp <- handleData state recvdata
	forM_ resp processReply
	loop
    loop

    where
	sendData handle frame = do
	    putTraceMsg ("sending " ++ (show $ BS8.length raw) ++ " bytes")
	    hPut handle raw
	    hFlush handle
	    where
		raw = SP.serialize frame
	
	processReply resp = case resp of
				SendFrames frames -> do
				    putTraceMsg $ "--> sending " ++ (show frames)
				    mapM_ (\fr -> sendData handle fr) frames
				Ignore -> do
				    print "Ignoring..."

handleData :: SpdyConnectionState -> ByteString -> IO [SpdyFrameResponse]
handleData state bs = do
    case SP.parse bs of
	Nothing -> fail $ "cannot decode " ++ (show $ BS.length bs)
	Just (frame, rest) -> do
	    putTraceMsg $ "received frame " ++ show frame ++ " rest " ++ show rest
	    handleParseResult frame rest
    where
	handleParseResult frame 0 = do
					res <- handleFrame frame state
					return [res]
	handleParseResult frame rest = 
	    handleFrame frame state >>= (\res -> do
					    nextRes <- handleData state (BS.drop (BS.length bs - rest) bs)
					    return $ res:nextRes)


dataReply id payload = Data id (Set.fromList [FLAG_FIN]) payload
    
    
handleFrame :: SP.Frame -> SpdyConnectionState -> IO SpdyFrameResponse
handleFrame fr@(SynStream header prio id assoc headers) state = do
    hd <- SP.inflateNvHeaders (zlibInflate state) headers
    compressedHeaders <- SP.deflateNvHeaders (zlibDeflate state) newHeaders
    
    -- insecure by intention
    payload <- BS.readFile ("public/" ++ (hd ! "url"))
    
    putTraceMsg $ "request headers: " ++ (show hd)
    return $ SendFrames [ synReply compressedHeaders
			, dataReply id payload]
    where
	synReply compressedHeaders = SynReply (frHeader compressedHeaders) id compressedHeaders
	newHeaders = Map.fromList [("status","200 OK")
				  ,("version", "HTTP/1.1")
				  ,("content-type", "text/html; charset=UTF-8")]

	frHeader compressedHeaders = ControlFrameHeader  2 (Set.fromList []) $ headerLength compressedHeaders
	headerLength compressedHeaders = 6 + (fromIntegral $ BS.length compressedHeaders)


handleFrame fr@(Ping frHeader id) state = do
    return $ SendFrames [fr]

handleFrame fr _ = do
    putTraceMsg $ "received an unimplemented frame " ++ show fr
    return Ignore

zlibDictionary = SU8.fromString "optionsgetheadpostputdeletetraceacceptaccept-charsetaccept-encodingaccept-\
                \languageauthorizationexpectfromhostif-modified-sinceif-matchif-none-matchi\
		\f-rangeif-unmodifiedsincemax-forwardsproxy-authorizationrangerefererteuser\
		\-agent10010120020120220320420520630030130230330430530630740040140240340440\
		\5406407408409410411412413414415416417500501502503504505accept-rangesageeta\
		\glocationproxy-authenticatepublicretry-afterservervarywarningwww-authentic\
		\ateallowcontent-basecontent-encodingcache-controlconnectiondatetrailertran\
		\sfer-encodingupgradeviawarningcontent-languagecontent-lengthcontent-locati\
		\oncontent-md5content-rangecontent-typeetagexpireslast-modifiedset-cookieMo\
		\ndayTuesdayWednesdayThursdayFridaySaturdaySundayJanFebMarAprMayJunJulAugSe\
		\pOctNovDecchunkedtext/htmlimage/pngimage/jpgimage/gifapplication/xmlapplic\
		\ation/xhtmltext/plainpublicmax-agecharset=iso-8859-1utf-8gzipdeflateHTTP/1\
		\.1statusversionurl\0"

initDeflate :: IO Zlib.Deflate
initDeflate = Zlib.initDeflateWithDictionary 6 zlibDictionary defaultWindowBits
	

initInflate :: IO Zlib.Inflate
initInflate = Zlib.initInflateWithDictionary defaultWindowBits zlibDictionary

	    
main = serveSpdy
