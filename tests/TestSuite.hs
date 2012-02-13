import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit

import qualified Data.ByteString as BS
import Network.SPDY
import Network.SPDY.Frame as SP
import Data.List
import Data.Maybe
import qualified Data.ByteString.UTF8 as SU8
import qualified Data.Map as Map
import qualified Data.Set as Set
import System.IO.Unsafe (unsafePerformIO)
import qualified Codec.Zlib as Zlib

main :: IO ()
main = defaultMain tests

tests = [testGroup "FrameParsing" [
	      testCase "Noop" testNoop
	    , testCase "Ping" testPing
	    , testCase "SynStream" testSynStream
	    , testCase "SynReply" testSynReply
	    , testCase "RstStream" testRstStream
	    , testCase "GoAway" testGoAway
	    , testCase "Headers" testHeaders
	    , testCase "SynStreamHeaders" testSynStreamHeaders
	    , testCase "Data" testData]
	, testGroup "Zlib" [
	      testCase "Deflate" testDeflate]
	, testGroup "Serialization <-> Deserialization" testSerializeDeserializeAll
        , testGroup "FrameSerialization" [
	    testCase "Data" testDataPut
	]]

testParseFrame :: BS.ByteString -> SP.Frame -> Assertion
testParseFrame bs frame = Just frame @?= SP.parse bs

testNoop = testParseFrame bs fr
    where
	bs = (BS.pack [128, 2, 0, 5, 0, 0, 0, 0]) 
	fr = Noop (ControlFrameHeader 2 Set.empty 0)

testHeaders = testParseFrame bs fr
    where
	fr = Headers (ControlFrameHeader 2 Set.empty 46) 1 headers
	rawHeaders = [ 0x78, 0xbb, 0xdf, 0xa2, 0x51, 0xb2, --Deflated Name/Value pairs
                       0x62, 0x60, 0x62, 0x60, 0x01, 0xe5, 0x12,
		       0x06, 0x4e, 0x50, 0x50, 0xe6, 0x80, 0x99,
		       0x6c, 0xc9, 0xa5, 0xc5, 0x25, 0xf9, 0xb9,
                       0x0c, 0x8c, 0x86, 0x00, 0x00, 0x00, 0x00, 0xff, 0xff]
	headers = BS.pack rawHeaders
	bs = BS.pack $ [ 128, 2, 0, 8,
		       0, 0, 0, 46, 
		       0x00, 0x00, 0x00, 0x01, -- Stream ID
		       0x00, 0x00  ] ++ rawHeaders -- Priority + Unused

testPing = testParseFrame bs fr
    where
	fr = Ping (ControlFrameHeader 2 Set.empty 4) 123
	bs = BS.pack [128, 2, 0, 6
		     , 0, 0, 0, 4 
        	     , 0, 0, 0, 123]

testRstStream = testParseFrame bs fr
    where
	fr = RstStream (ControlFrameHeader 2 Set.empty 8) 1 ProtocolError
	headers = Map.empty 
	bs = BS.pack [ 128, 2, 0, 3,
		       0, 0, 0, 8, 
		       0x00, 0x00, 0x00, 0x01, -- Stream ID
		       0x00, 0x00, 0x00, 0x01 -- Status Code
		     ]

testGoAway = testParseFrame bs fr
    where
	fr = GoAway (ControlFrameHeader 2 Set.empty 4) 42
	bs = BS.pack [ 128, 2, 0, 7,
		       0, 0, 0, 4, 
		       0x00, 0x00, 0x00, 42 -- Stream ID
		     ]

testData = testParseFrame bs fr
    where
	fr = Data 1 (Set.fromList [FLAG_FIN]) (BS.pack rawData)
	rawData = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
	bs = BS.pack $ [ 0, 0, 0, 1
		       , 1, 0, 0, 10] ++ rawData

testSynStream = testParseFrame bs fr
    where
	fr = SynStream (ControlFrameHeader 2 Set.empty 46) 0 1 Nothing headers
	rawHeaders = [ 0x78, 0xbb, 0xdf, 0xa2, 0x51, 0xb2, --Deflated Name/Value pairs
                       0x62, 0x60, 0x62, 0x60, 0x01, 0xe5, 0x12,
		       0x06, 0x4e, 0x50, 0x50, 0xe6, 0x80, 0x99,
		       0x6c, 0xc9, 0xa5, 0xc5, 0x25, 0xf9, 0xb9,
                       0x0c, 0x8c, 0x86, 0x00, 0x00, 0x00, 0x00, 0xff, 0xff]
	headers = BS.pack rawHeaders
	bs = BS.pack $ [ 128, 2, 0, 1,
		       0, 0, 0, 46, 
		       0x00, 0x00, 0x00, 0x01, -- Stream ID
		       0x00, 0x00, 0x00, 0x00, -- Associated Stream ID
		       0x00, 0x00  ] ++ rawHeaders -- Priority + Unused
		       

assertHeadersEqual :: BS.ByteString -> NvHeaders -> Assertion
assertHeadersEqual bs headers = do
    let decodedHeaders = do
	infl <- initInflate
	SP.inflateNvHeaders infl bs
    
    unsafePerformIO decodedHeaders @?= headers

demoHeaders = Map.fromList  [("host", "localhost")
	         	    ,("custom", "1")]

testSynStreamHeaders = do
    assertHeadersEqual (SP.headers frame) demoHeaders
    where
	bs = BS.pack $ [ 128, 2, 0, 1,
		       0, 0, 0, 46, 
		       0x00, 0x00, 0x00, 0x01, -- Stream ID
		       0x00, 0x00, 0x00, 0x00, -- Associated Stream ID
		       0x00, 0x00, 0x78, 0xbb, 0xdf, 0xa2, 0x51, 0xb2, --Deflated Name/Value pairs
                       0x62, 0x60, 0x62, 0x60, 0x01, 0xe5, 0x12,
		       0x06, 0x4e, 0x50, 0x50, 0xe6, 0x80, 0x99,
		       0x6c, 0xc9, 0xa5, 0xc5, 0x25, 0xf9, 0xb9,
                       0x0c, 0x8c, 0x86, 0x00, 0x00, 0x00, 0x00, 0xff, 0xff]
	frame = fromJust $ SP.parse bs

testSynReply = do
    assertEqual "" fr frame
    assertHeadersEqual (SP.headers frame) demoHeaders

    where
	frame = fromJust $ SP.parse bs
	headers = [0x78, 0xbb, 0xdf, 0xa2, 0x51, 0xb2, -- Deflated Name/Value pairs
		   0x62, 0x60, 0x62, 0x60, 0x01, 0xe5, 0x12,
		   0x06, 0x4e, 0x50, 0x50, 0xe6, 0x80, 0x99,
		   0x6c, 0xc9, 0xa5, 0xc5, 0x25, 0xf9, 0xb9,
		   0x0c, 0x8c, 0x86, 0x00, 0x00, 0x00, 0x00, 0xff, 0xff]
	fr = SynReply (ControlFrameHeader 2 Set.empty 46) 1 (BS.pack headers)
	bs = BS.pack $ [128, 2, 0, 2,
		        0, 0, 0, 46, 
			0x00, 0x00, 0x00, 0x01, -- Stream ID
			0x00, 0x00] ++ headers
			
	
testDataPut = (SP.serialize fr) @?= bs
    where
	payload = [1, 2, 3, 4]
	bs = BS.pack $ [0, 0, 0, 42,
		        1, 0, 0, 4] ++ payload
	fr = Data 42 (Set.fromList [FLAG_FIN]) (BS.pack payload)

testSerializeDeserializeAll =
    map makeTestCase frames
    where
	makeTestCase = \frame -> testCase (show frame) (testSerializeDeserialize frame)
	payload = BS.pack [1, 2, 3, 4]
	headers = BS.pack [1, 2, 3, 4]
	controlHeader = (ControlFrameHeader 2 Set.empty 14)
	frames = [ Data 42 (Set.fromList []) payload
		 , SynStream controlHeader 1 42 Nothing headers 
		 , SynReply controlHeader 1 headers
		 , GoAway controlHeader 1
		 , RstStream controlHeader 1 ProtocolError
		 , Noop controlHeader
		 ]
 
testSerializeDeserialize :: Frame -> Assertion
testSerializeDeserialize fr = (parse $ SP.serialize fr) @?= Just fr

testDeflate :: Assertion
testDeflate = do
	headers <- compress demoHeaders >>= (\bs -> inflate bs)
	unsafePerformIO headers @?= demoHeaders
    where	
	compress headers = do 
	    deflate <- initDeflate
	    SP.deflateNvHeaders deflate headers 
	inflate bs = do
	    inflate <- initInflate
	    return $ SP.inflateNvHeaders inflate bs
	
