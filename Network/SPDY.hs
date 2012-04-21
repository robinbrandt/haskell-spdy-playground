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
	    
	    forkIO $ procMessage lock connsock clientaddr
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

procMessage lock connsock clientaddr = do
    state <- initState

    let loop = do
	recvdata <- readMessage connsock 1024
	putTraceMsg ("received " ++ (show $ BS8.length recvdata ) ++ " bytes")
	resp <- handleData state recvdata
	case resp of
	    SendFrames frames -> do
				putTraceMsg $ "--> sending " ++ (show frames)
				mapM_ (\fr -> sendData connsock fr) frames
				loop
	    Ignore -> do
			print "Ignoring..."
			loop
	    Close -> sClose connsock
    loop

    where
	sendData connsock frame = do
	    putTraceMsg ("sending " ++ (show $ BS8.length raw) ++ " bytes")
	    send connsock raw
	    where
		raw = SP.serialize frame
	    
	readMessage connsock buflen = do
	    part <- recv connsock buflen
	    if (BS.length part) < buflen
	     then
		return part
	     else do
		rest <- readMessage connsock buflen
		return $ part `append` rest

handleData :: SpdyConnectionState -> ByteString -> IO SpdyFrameResponse
handleData state bs = do
    case SP.parse bs of
	Nothing -> fail $ "cannot decode " ++ (show $ BS.length bs)
	Just frame -> do
	    print frame
	    handleFrame frame state

dataReply id payload = Data id (Set.fromList [FLAG_FIN]) $ BS8.pack payload
    
    
handleFrame :: SP.Frame -> SpdyConnectionState -> IO SpdyFrameResponse
handleFrame fr@(SynStream header prio id assoc headers) state = do
    hd <- SP.inflateNvHeaders (zlibInflate state) headers
    compressedHeaders <- SP.deflateNvHeaders (zlibDeflate state) newHeaders
    
    putTraceMsg $ "request headers: " ++ (show hd)
    return $ SendFrames [ synReply compressedHeaders
			, dataReply id "<html><body>Hello World</body></html>"]
    where
	synReply compressedHeaders = SynReply (frHeader compressedHeaders) id compressedHeaders
	newHeaders = Map.fromList [("status","200 OK")
				  ,("version", "HTTP/1.1")
				  ,("content-type", "text/html; charset=UTF-8")
				  ,("date", "Mon, 23 May 2005 22:38:34 GMT")
				  ,("server", "Apache/1.3.3.7 (Unix) (Red-Hat/Linux)")]

	frHeader compressedHeaders = ControlFrameHeader  2 (Set.fromList []) $ headerLength compressedHeaders
	headerLength compressedHeaders = 6 + (fromIntegral $ BS.length compressedHeaders)


handleFrame fr@(Ping frHeader id) state = do
    return $ SendFrames [fr]

handleFrame fr _ = do
    print fr
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
