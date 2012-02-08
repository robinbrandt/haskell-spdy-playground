module Network.SPDY
    where

import Data.ByteString as BS hiding (head) 
import Data.Set as Set
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

serveSpdy :: IO ()
serveSpdy = withSocketsDo $
    do
	addrinfos <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]})) Nothing (Just "1111")
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
		zlibInflate :: Zlib.Inflate }	

data SpdyFrameResponse = SendFrame {
			  frame :: SP.Frame }
		       | Close

initState :: IO SpdyConnectionState
initState = do
    inflate <- initInflate
    return $ SpdyConnectionState inflate

procMessage lock connsock clientaddr = do
    state <- initState

    let loop = do
	recvdata <- readMessage connsock 1024
	resp <- handleData state recvdata
	case resp of
	    SendFrame fr -> do
				print $ "--> sending " ++ (show fr)
				void $ send connsock $ SP.serialize fr
				loop
	    Close -> sClose connsock
    loop

    where
	readMessage connsock buflen = do
	    part <- recv connsock buflen
	    if (BS.length part) < buflen then
		return part
	    else do
		rest <- readMessage connsock buflen
		return $ part `append` rest

handleData :: SpdyConnectionState -> ByteString -> IO SpdyFrameResponse
handleData state bs = do
    case SP.parse bs of
	Nothing -> fail "cannot decode"
	Just frame -> do
	    print frame
	    handleFrame frame state
    
handleFrame :: SP.Frame -> SpdyConnectionState -> IO SpdyFrameResponse
handleFrame fr@(SynStream header prio id assoc headers) state = do
    hd <- SP.inflateNvHeaders (zlibInflate state) headers
    print hd
    return $ SendFrame synReply
    where
	synReply = SynReply frHeader id hdrs
	frHeader = ControlFrameHeader  2 (Set.fromList []) 8
	hdrs = BS.empty

handleFrame fr@(Ping frHeader id) state = do
    return $ SendFrame fr

handleFrame fr _ = do
    print fr
    return Close

initInflate :: IO Zlib.Inflate
initInflate = inflate
    where
	inflate = Zlib.initInflateWithDictionary (Zlib.WindowBits 15) dict
	dict = SU8.fromString "optionsgetheadpostputdeletetraceacceptaccept-charsetaccept-encodingaccept-\
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

	    
main = serveSpdy
